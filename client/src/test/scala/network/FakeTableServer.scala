package network

import org.java_websocket.WebSocket
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.server.WebSocketServer
import java.net.{InetSocketAddress, URI}
import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, Semaphore, TimeUnit}
import scala.jdk.CollectionConverters.*

/** Minimal WebSocket server for the client tests: records what it receives and can write to,
 * or drop, its clients. (Queues use `offer`, which never blocks nor throws when interrupted.)
 */
final class FakeTableServer:
  private val received     = LinkedBlockingQueue[String]()
  private val connected    = Semaphore(0)
  private val disconnected = Semaphore(0)
  private val started      = CountDownLatch(1)

  private val server = new WebSocketServer(InetSocketAddress(0)): // port 0: free port chosen by the OS
    override def onStart(): Unit = started.countDown()
    override def onOpen(socket: WebSocket, handshake: ClientHandshake): Unit = connected.release()
    override def onClose(socket: WebSocket, code: Int, reason: String, remote: Boolean): Unit = disconnected.release()
    override def onMessage(socket: WebSocket, text: String): Unit = received.offer(text)
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

