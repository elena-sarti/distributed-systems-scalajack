package network

import cats.effect.{Deferred, IO, Resource}
import cats.effect.std.{Dispatcher, Queue, Semaphore}
import protocol.{ClientMessage, MessageCodec}
import java.net.URI
import java.net.http.{HttpClient, WebSocket}
import java.nio.ByteBuffer
import java.time.Duration as JDuration
import java.util.concurrent.CompletionStage
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.*

/** Opens a [[ServerConnection]] over WebSocket, using the JDK's built-in client. */
object WebSocketConnection:
  private val http           = HttpClient.newHttpClient() // shared: it owns threads, one is enough
  private val ConnectTimeout = JDuration.ofSeconds(5)
  private val CloseTimeout   = 2.seconds

  /** The connection lives as long as the [[Resource]] is in use and is always closed afterward.
   *
   * A table node that crashes closes the connection, but one that freezes or is cut off by the network says nothing:
   * so the client pings it every `pingEvery` and takes the connection as lost when nothing has come back for `silence`,
   * the same failure detector the nodes use among them.
   *
   * @param token the session token given by the lobby: the tables accept only the clients that show one */
  def connect(uri: URI, token: Option[String] = None,
              pingEvery: FiniteDuration = 2.seconds, silence: FiniteDuration = 6.seconds): Resource[IO, ServerConnection] =
    for
      // Bridge between the callbacks and cats-effect: it runs the effects
      // submitted by the callbacks one at a time and in order, so messages are not reordered.
      dispatcher <- Dispatcher.sequential[IO]
      // Where the callbacks put what happens on the connection and receive takes it from.
      events     <- Resource.eval(Queue.unbounded[IO, ConnectionEvent])
      // Completed once, when the connection is over: lets `send` fail fast and `close` wait for the handshake.
      closed     <- Resource.eval(Deferred[IO, Unit])
      // One permit: only one send at a time.
      writer     <- Resource.eval(Semaphore[IO](1))
      listener    = EventListener(dispatcher, events, closed)
      socket     <- Resource.make(open(uri, token, listener))(close(_, closed))
      _          <- watch(socket, listener, writer, pingEvery, silence).background
    yield WebSocketServerConnection(socket, events, closed, writer)

  /** Pings the table and gives the connection up when the table has been silent for too long. */
  private def watch(socket: WebSocket, listener: EventListener, writer: Semaphore[IO],
                    pingEvery: FiniteDuration, silence: FiniteDuration): IO[Unit] =
    val ping = writer.permit.surround(IO.fromCompletableFuture(IO(socket.sendPing(ByteBuffer.allocate(0)))).void).attempt.void
    (IO.sleep(pingEvery) >> IO(listener.silentFor)).flatMap: quiet =>
      if quiet > silence then IO(listener.lost(s"The table has not answered for ${silence.toSeconds} s")) >> IO(socket.abort())
      else ping >> watch(socket, listener, writer, pingEvery, silence)

  /** Starts the handshake. buildAsync returns immediately with a future: fromCompletableFuture
   * turns it into an IO that suspends (without blocking a thread) until the connection is ready or fails. */
  private def open(uri: URI, token: Option[String], listener: WebSocket.Listener): IO[WebSocket] =
    IO.fromCompletableFuture(IO:
      val builder = http.newWebSocketBuilder().connectTimeout(ConnectTimeout)
      token.foreach(value => builder.header("Authorization", s"Bearer $value"))
      builder.buildAsync(uri, listener))

  /** Polite close: send a Close frame, give the server a moment to answer, then drop the socket anyway.
   * Errors while sending are ignored: if the connection is already gone there is nothing left to close. */
  private def close(socket: WebSocket, closed: Deferred[IO, Unit]): IO[Unit] =
    IO.fromCompletableFuture(IO(socket.sendClose(WebSocket.NORMAL_CLOSURE, "bye"))).attempt *>
      closed.get.timeoutTo(CloseTimeout, IO.unit) *>
      IO(socket.abort())

  /** Turns the JDK callbacks (invoked on JDK threads, at times we do not control)
   * into [[ConnectionEvent]]s put in a queue. */
  private final class EventListener(dispatcher: Dispatcher[IO], events: Queue[IO, ConnectionEvent], closed: Deferred[IO, Unit])
    extends WebSocket.Listener:

    private val partial = StringBuilder() //a text message may arrive split in several fragments
    private val lastHeard = AtomicLong(System.nanoTime())

    /** How long since anything came from the table. */
    def silentFor: FiniteDuration = (System.nanoTime() - lastHeard.get).nanos

    def lost(reason: String): Unit = finish(reason)

    override def onPong(ws: WebSocket, message: ByteBuffer): CompletionStage[?] =
      lastHeard.set(System.nanoTime())
      ws.request(1)
      null

    override def onText(ws: WebSocket, data: CharSequence, last: Boolean): CompletionStage[?] =
      lastHeard.set(System.nanoTime())
      partial.append(data)
      if last then  //the message is complete: decode it and publish it
        val text = partial.toString
        partial.clear()
        dispatcher.unsafeRunAndForget(events.offer(decode(text)))
      ws.request(1) //the JDK delivers one message at a time: ask for the next one
      null //nothing more to wait for

    override def onClose(ws: WebSocket, statusCode: Int, reason: String): CompletionStage[?] =
      ws.sendClose(WebSocket.NORMAL_CLOSURE, "") //complete the closing handshake
      finish(s"Connection closed by the server ($statusCode)")
      null

    override def onError(ws: WebSocket, error: Throwable): Unit =
      finish(s"Connection lost: ${error.getMessage}")

    /** Never throws: a malformed frame becomes an `Invalid` event. */
    private def decode(text: String): ConnectionEvent =
      MessageCodec.decodeServerMessage(text) match
        case Right(message) => ConnectionEvent.Received(message)
        case Left(reason)   => ConnectionEvent.Invalid(reason)

    /** Marks the connection as over and publishes a single Disconnected.
     * `complete` returns true only the first time, so even if both `onClose` and `onError`
     * fire, the queue gets exactly one terminal event.*/
    private def finish(reason: String): Unit =
      dispatcher.unsafeRunAndForget:
        closed.complete(()).flatMap: firstTime =>
          IO.whenA(firstTime)(events.offer(ConnectionEvent.Disconnected(reason)))

  /** The [[ServerConnection]] handed to the client code: a thin layer over the socket and the queue. */
  private final class WebSocketServerConnection(
                                                 socket: WebSocket,
                                                 events: Queue[IO, ConnectionEvent],
                                                 closed: Deferred[IO, Unit],
                                                 writer: Semaphore[IO]
                                               ) extends ServerConnection:

    def send(message: ClientMessage): IO[Unit] =
      writer.permit.surround: //it serializes the sends: fails if two overlap
        closed.tryGet.flatMap:
          case Some(_) => IO.raiseError(ConnectionClosedException())
          case None    => IO.fromCompletableFuture(IO(socket.sendText(MessageCodec.encode(message), true))).void

    /** `take` suspends the caller while the queue is empty, without blocking a thread. */
    def receive: IO[ConnectionEvent] =
      events.take.flatTap:
        // he terminal event is put back, so every later call sees it too instead of waiting forever.
        case terminal: ConnectionEvent.Disconnected => events.offer(terminal) //keep it for the next calls
        case _                                      => IO.unit

