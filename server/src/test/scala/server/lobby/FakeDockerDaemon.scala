package server.lobby

import java.io.{BufferedReader, InputStreamReader}
import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{Channels, ServerSocketChannel, SocketChannel}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentLinkedQueue
import scala.util.control.NonFatal

/** A stand-in for the Docker daemon: listens on a Unix socket, records the first line of every request
 * (e.g. `POST /containers/table-2/start HTTP/1.1`) and answers with the status the test chose, so the
 * provisioner can be tested without Docker.
 *
 * Creating it throws if the system has no Unix domain sockets: the tests that need it are then cancelled. */
final class FakeDockerDaemon:
  private val directory = Files.createTempDirectory("fake-docker")
  val socket: Path      = directory.resolve("docker.sock")
  private val server    = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
  server.bind(UnixDomainSocketAddress.of(socket))

  /** The status of the next answers. */
  @volatile var status: Int = 204
  /** The containers that do not exist: the requests about them get 404, whatever `status` is. */
  @volatile var missing: Set[String] = Set.empty
  /** If `false` the daemon accepts the request and then stays silent (a daemon that hangs). */
  @volatile var answers: Boolean = true
  val requests = ConcurrentLinkedQueue[String]()

  private val silentConnections = ConcurrentLinkedQueue[SocketChannel]()

  private val acceptor = Thread(() => acceptLoop(), "fake-docker-daemon")
  acceptor.setDaemon(true)
  acceptor.start()

  private def acceptLoop(): Unit =
    try while true do serve(server.accept())
    catch case NonFatal(_) => () // the server was closed: the test is over

  private def serve(channel: SocketChannel): Unit =
    val in = BufferedReader(InputStreamReader(Channels.newInputStream(channel), UTF_8))
    val request = in.readLine()
    requests.add(request)
    val answer = if missing.exists(name => request.contains(s"/containers/$name/")) then 404 else status
    Iterator.continually(in.readLine()).takeWhile(line => line != null && line.nonEmpty).foreach(_ => ()) // skip the headers
    if answers then
      channel.write(ByteBuffer.wrap(s"HTTP/1.1 $answer Reply\r\nContent-Length: 0\r\nConnection: close\r\n\r\n".getBytes(UTF_8)))
      channel.close()
    else silentConnections.add(channel)

  def stop(): Unit =
    server.close()
    silentConnections.forEach(_.close())
    Files.deleteIfExists(socket)
    Files.deleteIfExists(directory)
