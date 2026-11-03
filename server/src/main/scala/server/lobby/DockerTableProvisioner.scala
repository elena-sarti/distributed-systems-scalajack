package server.lobby

import java.io.{BufferedReader, InputStreamReader}
import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{Channels, SocketChannel}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path
import java.time.Duration
import java.util.concurrent.{Executors, TimeUnit}
import scala.util.Using
import scala.util.control.NonFatal

/** Starts the containers of the tables by asking the Docker daemon (`POST /containers/{name}/start` on its
 * Unix socket). The containers must already exist: they are created, not started, when the system is set
 * up (see docker-compose.yml). Starting a container that is already running is not an error.
 *
 * To reach the daemon the lobby needs its socket mounted in the container, which gives the lobby wide
 * control of Docker on the host. It is a choice for the simulation of the nodes; a real deployment would use
 * an orchestrator with narrower permissions.
 *
 * JDK 16 or later talks to Unix sockets by itself, and the request is so small that it is written by
 * hand instead of adding a Docker library.
 *
 * @param socket        the Unix socket of the daemon.
 * @param timeout       the longest the whole request may take: a daemon that does not answer must not block the
 *                      dispatcher, which waits for this call.
 * @param containerName the name of the container of a table (by default the id of the table itself).
 */
final class DockerTableProvisioner(socket: Path = Path.of("/var/run/docker.sock"),
                                   timeout: Duration = Duration.ofSeconds(5),
                                   containerName: String => String = identity) extends TableProvisioner:
  import DockerTableProvisioner.*

  def start(tableId: String): Boolean =
    val name = containerName(tableId)
    if !TableProvisioner.isValidId(name) then
      System.err.println(s"Cannot start table $tableId: '$name' is not a valid container name")
      false
    else
      post(s"/containers/$name/start") match
        case Some(204 | 304) => true // started now, or it was already running
        case Some(404) =>
          System.err.println(s"Cannot start table $tableId: container '$name' does not exist (it must be created first)")
          false
        case other =>
          System.err.println(s"Cannot start table $tableId: ${other.fold("no answer from Docker")(status => s"Docker answered $status")}")
          false

  /** The status of the answer, or `None` if there is none (no daemon, no permission, timeout...). */
  private def post(path: String): Option[Int] =
    try
      Using.resource(SocketChannel.open(StandardProtocolFamily.UNIX)): channel =>
        // Reading from the channel has no timeout: closing it from outside is what interrupts a daemon that is silent.
        val closer: Runnable = () => channel.close() // typed as Runnable: schedule also accepts a Callable, and the lambda fits both
        val watchdog = Watchdog.schedule(closer, timeout.toMillis, TimeUnit.MILLISECONDS)
        try
          channel.connect(UnixDomainSocketAddress.of(socket))
          val request = ByteBuffer.wrap(s"POST $path HTTP/1.1\r\nHost: docker\r\nContent-Length: 0\r\nConnection: close\r\n\r\n".getBytes(UTF_8))
          while request.hasRemaining do channel.write(request)
          val statusLine = BufferedReader(InputStreamReader(Channels.newInputStream(channel), UTF_8)).readLine()
          Option(statusLine).flatMap(_.split(' ').lift(1)).flatMap(_.toIntOption) // "HTTP/1.1 204 No Content"
        finally watchdog.cancel(false)
    catch case NonFatal(_) => None

object DockerTableProvisioner:
  /** Closes the connections that take too long. A daemon thread: it must not keep the JVM alive. */
  private val Watchdog = Executors.newSingleThreadScheduledExecutor: runnable =>
    val thread = Thread(runnable, "docker-watchdog")
    thread.setDaemon(true)
    thread
