package network

import org.java_websocket.WebSocket
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.server.WebSocketServer
import java.net.{InetSocketAddress, URI}
import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, Semaphore, TimeUnit}
import scala.jdk.CollectionConverters.*

/** Minimal WebSocket server for the client tests: records what it receives and can write to
 * or drop its clients. */
final class FakeTableServer:
  private val received     = LinkedBlockingQueue[String]()
  private val connected    = Semaphore(0)
  private val disconnected = Semaphore(0)
  private val started      = CountDownLatch(1)

  /** The `Authorization` header of the last handshake, if it had one. */
  @volatile var lastAuthorization: Option[String] = None
  /** What the server answers to a message from a client, if anything (by default it answers nothing). */
  @volatile var reply: String => Option[String] = _ => None

  private val server = new WebSocketServer(InetSocketAddress(0)): // port 0: free port chosen by the OS
    override def onStart(): Unit = started.countDown()
    override def onOpen(socket: WebSocket, handshake: ClientHandshake): Unit =
      lastAuthorization = Option(handshake.getFieldValue("Authorization")).filter(_.nonEmpty)
      connected.release()
    override def onClose(socket: WebSocket, code: Int, reason: String, remote: Boolean): Unit = disconnected.release()
    override def onMessage(socket: WebSocket, text: String): Unit =
      received.offer(text)
      reply(text).foreach(socket.send)
    override def onError(socket: WebSocket, error: Exception): Unit = ()

  def start(): Unit =
    server.start()
    if !started.await(5, TimeUnit.SECONDS) then throw AssertionError("The fake server did not start")

  def stop(): Unit = server.stop(1000)

  /** Valid only while the server is running. */
  def uri: URI = URI.create(s"ws://localhost:${server.getPort}")

  def sendToClients(text: String): Unit = server.broadcast(text)
  def disconnectClients(): Unit         = server.getConnections.asScala.foreach(_.close(1001))
  def connectionCount: Int              = server.getConnections.size

  def awaitClientConnected(): Unit    = acquire(connected, "a client to connect")
  def awaitClientDisconnected(): Unit = acquire(disconnected, "a client to disconnect")

  def nextReceived(): String =
    Option(received.poll(5, TimeUnit.SECONDS)).getOrElse(throw AssertionError("Timed out waiting for a message"))

  private def acquire(semaphore: Semaphore, what: String): Unit =
    if !semaphore.tryAcquire(5, TimeUnit.SECONDS) then throw AssertionError(s"Timed out waiting for $what")

