package server

import org.java_websocket.WebSocket
import org.java_websocket.drafts.Draft
import org.java_websocket.exceptions.{InvalidDataException, WebsocketNotConnectedException}
import org.java_websocket.framing.CloseFrame
import org.java_websocket.handshake.{ClientHandshake, ServerHandshakeBuilder}
import org.java_websocket.server.WebSocketServer
import protocol.{ClientMessage, MessageCodec, ServerMessage}
import server.auth.SessionTokens
import java.net.InetSocketAddress
import java.util.UUID
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, TimeUnit}

/** WebSocket transport of a table node. It turns every text frame into a [[protocol.ClientMessage]] for the
 * [[SessionHandler]] and knows nothing about the game. The opening handshake must carry `Authorization: Bearer <token>`
 * with a valid session token, and the connection speaks for that user only.
 *
 * @param port    the port to listen on, or 0 for a free port chosen by the OS.
 * @param handler the table logic that reacts to connections and messages.
 * @param tokens  checks the session tokens.
 */
final class TableEndpoint(port: Int, handler: SessionHandler, tokens: SessionTokens):
  import TableEndpoint.*

  private val connections = ConcurrentHashMap[WebSocket, WsConnection]()
  private val started     = CountDownLatch(1)
  @volatile private var startupError: Option[Exception] = None
  private val server = Server()

  /** Starts listening and returns when the endpoint is ready. Throws if it cannot start. */
  def start(): Unit =
    server.start()
    val signalled = started.await(StartTimeoutSeconds, TimeUnit.SECONDS)
    startupError match
      case Some(error)          => throw IllegalStateException(s"Cannot start the WebSocket endpoint on port $port", error)
      case None if !signalled   => throw IllegalStateException(s"WebSocket endpoint not started within ${StartTimeoutSeconds}s")
      case None                 => ()

  /** Closes every connection and releases the port. Calling it twice is harmless. */
  def stop(): Unit = server.stop(StopTimeoutMillis)

  /** Tells the handler that a connection is closed. The interrupt flag set by the library while stopping is cleared during
   * the callback and restored afterward, so the notification is not lost. */
  private def notifyDisconnected(connection: Connection): Unit =
    val wasInterrupted = Thread.interrupted() // reads and clears the flag
    try handler.onDisconnected(connection)
    finally if wasInterrupted then Thread.currentThread().interrupt()
  def boundPort: Int       = server.getPort
  def connectionCount: Int = connections.size

  private final class Server extends WebSocketServer(InetSocketAddress(port)):

    override def onStart(): Unit = started.countDown()

    /** Runs before the connection is opened. Without a valid token the handshake fails and the handler never hears of the
     * client. The user is kept on the socket for [[onOpen]]. */
    override def onWebsocketHandshakeReceivedAsServer(socket: WebSocket, draft: Draft,
                                                      request: ClientHandshake): ServerHandshakeBuilder =
      val user = bearerToken(request.getFieldValue("Authorization")).flatMap(tokens.verify)
        .getOrElse(throw InvalidDataException(CloseFrame.POLICY_VALIDATION, NotLoggedIn))
      socket.setAttachment(user)
      super.onWebsocketHandshakeReceivedAsServer(socket, draft, request)

    override def onOpen(socket: WebSocket, handshake: ClientHandshake): Unit =
      val connection = WsConnection(socket, socket.getAttachment[String]())
      connections.put(socket, connection) // registered before the handler is told
      handler.onConnected(connection)

    override def onMessage(socket: WebSocket, text: String): Unit =
      Option(connections.get(socket)).foreach: connection =>
        MessageCodec.decodeClientMessage(text) match
          case Right(ClientMessage.JoinTable(name)) if name != connection.user =>
            connection.send(ServerMessage.JoinRejected(NotYourName))
          case Right(message)                                                  => handler.onMessage(connection, message)
          case Left(reason)                                                    => connection.send(ServerMessage.ErrorMessage(reason))

    override def onClose(socket: WebSocket, code: Int, reason: String, remote: Boolean): Unit =
      // removed before the handler is told: a closed connection is never counted again
      Option(connections.remove(socket)).foreach(notifyDisconnected)

    override def onError(socket: WebSocket, error: Exception): Unit =
      if socket == null && started.getCount > 0 then // the server itself could not start
        startupError = Some(error)
        started.countDown()
      else System.err.println(s"WebSocket error: ${error.getMessage}")

object TableEndpoint:
  val NotLoggedIn = "A valid session token is needed to sit at a table: log in to the lobby first"
  val NotYourName = "You can only sit down with the username you logged in with"

  private val StartTimeoutSeconds = 5L
  private val StopTimeoutMillis   = 1000

  private def bearerToken(header: String): Option[String] =
    Option(header).map(_.trim).filter(_.startsWith("Bearer ")).map(_.stripPrefix("Bearer ").trim).filter(_.nonEmpty)

  /** A connection over a WebSocket.
   *
   * @param user the logged-in user the connection speaks for.
   */
  private final class WsConnection(socket: WebSocket, val user: String) extends Connection:
    val id: String = UUID.randomUUID().toString
    def send(message: ServerMessage): Unit =
      try socket.send(MessageCodec.encode(message))
      catch case _: WebsocketNotConnectedException => () // closed in the meantime: onClose does the cleanup

