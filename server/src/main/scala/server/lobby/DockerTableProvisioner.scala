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

/** Starts the containers of the tables through the Docker daemon, on its Unix socket. The containers must already exist
 * and starting one that is running is not an error.
 * The lobby needs the socket mounted, which gives it wide control of Docker and is acceptable for a simulation.
 * The request is written by hand to avoid a Docker library.
 *
 * @param socket        the Unix socket of the daemon.
 * @param timeout       the longest the whole request may take.
 * @param containerName the name of the container of a table, by default its id.
 * @param backupName    the name of the container of the backup of a table, if it has one. It is started first and a
 *                      missing backup does not prevent the table from starting.
 */
final class DockerTableProvisioner(socket: Path = Path.of("/var/run/docker.sock"),
                                   timeout: Duration = Duration.ofSeconds(5),
                                   containerName: String => String = identity,
                                   backupName: String => Option[String] = _ => None) extends TableProvisioner:
  import DockerTableProvisioner.*

  def start(tableId: String): Boolean =
    backupName(tableId).foreach(startBackup(tableId, _))
    val name = containerName(tableId)
    if !TableProvisioner.isValidId(name) then
      System.err.println(s"Cannot start table $tableId: '$name' is not a valid container name")
      false
    else
      post(s"/containers/$name/start") match
        case Some(204 | 304) => true // started now, or it was already running
        case Some(404)       =>
          System.err.println(s"Cannot start table $tableId: container '$name' does not exist (it must be created first)")
          false
        case other           =>
          System.err.println(s"Cannot start table $tableId: ${other.fold("no answer from Docker")(status => s"Docker answered $status")}")
          false

  /** A backup that cannot be started does not keep the table down, since the table goes on without it once the lobby
   * agrees. So this only writes in the log. */
  private def startBackup(tableId: String, name: String): Unit =
    if !TableProvisioner.isValidId(name) then
      System.err.println(s"Cannot start the backup of table $tableId: '$name' is not a valid container name")
    else post(s"/containers/$name/start") match
      case Some(204 | 304) => ()
      case Some(404)       => System.out.println(s"Table $tableId has no backup: container '$name' does not exist")
      case other =>
        System.err.println(s"Cannot start the backup of table $tableId: ${other.fold("no answer from Docker")(status => s"Docker answered $status")}")

  /** The status of the answer, or `None` if there is none. */
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
  /** Closes the connections that take too long. Its thread is a daemon, so it does not keep the JVM alive. */
  private val Watchdog = Executors.newSingleThreadScheduledExecutor: runnable =>
    val thread = Thread(runnable, "docker-watchdog")
    thread.setDaemon(true)
    thread
