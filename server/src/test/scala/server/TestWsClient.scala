package server

import protocol.{ClientMessage, MessageCodec, ServerMessage}
import java.net.URI
import java.net.http.{HttpClient, WebSocket}
import java.util.concurrent.{CompletionStage, CountDownLatch, LinkedBlockingQueue, TimeUnit}

/** Minimal WebSocket client for tests, based on the JDK's built-in client. */
final class TestWsClient(uri: URI):
  private val inbox   = LinkedBlockingQueue[String]()
  private val closed  = CountDownLatch(1)
  private val partial = StringBuilder() // a text message may arrive split in several fragments

  private val listener = new WebSocket.Listener:
    override def onText(ws: WebSocket, data: CharSequence, last: Boolean): CompletionStage[?] =
      partial.append(data)
      if last then
        inbox.put(partial.toString)
        partial.clear()
      ws.request(1) // ask for the next message
      null
    override def onClose(ws: WebSocket, statusCode: Int, reason: String): CompletionStage[?] =
      ws.sendClose(WebSocket.NORMAL_CLOSURE, "") // complete the closing handshake
      closed.countDown()
      null
    override def onError(ws: WebSocket, error: Throwable): Unit = closed.countDown()

  private val socket: WebSocket =
    TestWsClient.http.newWebSocketBuilder().buildAsync(uri, listener).get(5, TimeUnit.SECONDS)

  def send(message: ClientMessage): Unit = sendRaw(MessageCodec.encode(message))
  def sendRaw(text: String): Unit        = socket.sendText(text, true).get(5, TimeUnit.SECONDS)

  def receive(): ServerMessage =
    val json = Option(inbox.poll(5, TimeUnit.SECONDS)).getOrElse(throw AssertionError("No message within 5 seconds"))
    MessageCodec.decodeServerMessage(json) match
      case Right(message) => message
      case Left(error)    => throw AssertionError(error)

  def close(): Unit          = socket.sendClose(WebSocket.NORMAL_CLOSURE, "bye").get(5, TimeUnit.SECONDS)
  def awaitClosed(): Boolean = closed.await(5, TimeUnit.SECONDS)

object TestWsClient:
  private val http = HttpClient.newHttpClient() // shared, so tests do not create one set of threads per client