package server

import org.java_websocket.WebSocket
import org.java_websocket.exceptions.WebsocketNotConnectedException
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.server.WebSocketServer
import protocol.{MessageCodec, ServerMessage}
import java.net.InetSocketAddress
import java.util.UUID
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, TimeUnit}

/** WebSocket transport of a table node: accepts connections, turns every text frame into a
 * [[protocol.ClientMessage]] and hands it to the [[SessionHandler]]. It knows nothing about the game.
 *
 * @param port    the port to listen on (0 = a free port chosen by the OS, see [[boundPort]]).
 * @param handler the table logic that reacts to connections and messages.
 */
final class TableEndpoint(port: Int, handler: SessionHandler):
  import TableEndpoint.*

  private val connections = ConcurrentHashMap[WebSocket, Connection]()
  private val started     = CountDownLatch(1)
  @volatile private var startupError: Option[Exception] = None
  private val server = Server()

  /** Starts listening; returns when the endpoint is ready, throws if it cannot start (ex. port in use). */
  def start(): Unit =
    server.start()
    val signalled = started.await(StartTimeoutSeconds, TimeUnit.SECONDS)
    startupError match
      case Some(error)          => throw IllegalStateException(s"Cannot start the WebSocket endpoint on port $port", error)
      case None if !signalled   => throw IllegalStateException(s"WebSocket endpoint not started within ${StartTimeoutSeconds}s")
      case None                 => ()

  /** Closes every connection and releases the port. Calling it twice is harmless. */
  def stop(): Unit = server.stop(StopTimeoutMillis)

  /** While the endpoint is stopping, the library interrupts its own thread just before calling
   * onClose for the last connection. An interrupted thread makes blocking calls (queue.put)
   * throw, which would silently lose the notification: the flag is cleared during the callback
   * and restored afterwards, so the library still shuts down as it expects.
   */
  private def notifyDisconnected(connection: Connection): Unit =
    val wasInterrupted = Thread.interrupted() // reads and clears the flag
    try handler.onDisconnected(connection)
    finally if wasInterrupted then Thread.currentThread().interrupt()
  def boundPort: Int       = server.getPort
  def connectionCount: Int = connections.size

  private final class Server extends WebSocketServer(InetSocketAddress(port)):

    override def onStart(): Unit = started.countDown()

    override def onOpen(socket: WebSocket, handshake: ClientHandshake): Unit =
      val connection = WsConnection(socket)
      connections.put(socket, connection) // registered before the handler is told
      handler.onConnected(connection)

    override def onMessage(socket: WebSocket, text: String): Unit =
      Option(connections.get(socket)).foreach: connection =>
        MessageCodec.decodeClientMessage(text) match
          case Right(message) => handler.onMessage(connection, message)
          case Left(reason)   => connection.send(ServerMessage.ErrorMessage(reason))

    override def onClose(socket: WebSocket, code: Int, reason: String, remote: Boolean): Unit =
      // removed before the handler is told: a closed connection is never counted again
      Option(connections.remove(socket)).foreach(notifyDisconnected)

    override def onError(socket: WebSocket, error: Exception): Unit =
      if socket == null && started.getCount > 0 then // the server itself could not start
        startupError = Some(error)
        started.countDown()
      else System.err.println(s"WebSocket error: ${error.getMessage}")

object TableEndpoint:
  private val StartTimeoutSeconds = 5L
  private val StopTimeoutMillis   = 1000

  private final class WsConnection(socket: WebSocket) extends Connection:
    val id: String = UUID.randomUUID().toString
    def send(message: ServerMessage): Unit =
      try socket.send(MessageCodec.encode(message))
      catch case _: WebsocketNotConnectedException => () // closed in the meantime: onClose does the cleanup

