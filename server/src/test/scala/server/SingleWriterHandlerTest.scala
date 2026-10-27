package server

import utils.TestExports.*
import protocol.{ClientMessage, ServerMessage}
import server.TestSupport.next
import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

class SingleWriterHandlerTest extends AnyFunSuite:

  private val connection = new Connection:
    val id = "test"
    def send(message: ServerMessage): Unit = ()

  private def withWriter(table: SessionHandler)(body: SingleWriterHandler => Unit): Unit =
    val writer = SingleWriterHandler(table)
    try body(writer)
    finally writer.stop()

  /** A slow table that measures how many messages it is processing at the same time. */
  private final class OverlapProbe(workMillis: Long) extends SessionHandler:
    private val inProgress = AtomicInteger(0)
    val maxInProgress      = AtomicInteger(0)
    val processed          = LinkedBlockingQueue[ClientMessage]()

    def onConnected(connection: Connection): Unit    = ()
    def onDisconnected(connection: Connection): Unit = ()
    def onMessage(connection: Connection, message: ClientMessage): Unit =
      val now = inProgress.incrementAndGet()
      maxInProgress.accumulateAndGet(now, (a, b) => math.max(a, b))
      Thread.sleep(workMillis) // long enough for another thread to overlap, if it could
      inProgress.decrementAndGet()
      processed.put(message)

  /** Delivers the messages from many threads that all start at the same moment. */
  private def sendConcurrently(writer: SingleWriterHandler, threads: Int, perThread: Int): Unit =
    val start = CountDownLatch(1)
    val senders = (0 until threads).map: t =>
      Thread: () =>
        start.await()
        (1 to perThread).foreach(i => writer.onMessage(connection, ClientMessage.PlaceBet(t * perThread + i)))
    senders.foreach(_.start())
    start.countDown()
    senders.foreach(_.join())

  test("two concurrent messages are processed in sequence, never in parallel"):
    val probe = OverlapProbe(workMillis = 50)
    withWriter(probe): writer =>
      sendConcurrently(writer, threads = 2, perThread = 1)
      Set(next(probe.processed), next(probe.processed)) shouldBe
        Set(ClientMessage.PlaceBet(1), ClientMessage.PlaceBet(2))
      probe.maxInProgress.get shouldBe 1

  test("many clients writing at the same time never overlap and no message is lost"):
    val probe = OverlapProbe(workMillis = 1)
    withWriter(probe): writer =>
      sendConcurrently(writer, threads = 8, perThread = 25)
      val processed = (1 to 200).map(_ => next(probe.processed))
      processed.toSet shouldBe (1 to 200).map(ClientMessage.PlaceBet(_)).toSet
      probe.maxInProgress.get shouldBe 1

  test("events are processed in the order they arrive"):
    val log = LinkedBlockingQueue[String]()
    val table = new SessionHandler:
      def onConnected(connection: Connection): Unit                        = log.put("connected")
      def onMessage(connection: Connection, message: ClientMessage): Unit = log.put(message.toString)
      def onDisconnected(connection: Connection): Unit                     = log.put("disconnected")
    withWriter(table): writer =>
      writer.onConnected(connection)
      (1 to 20).foreach(i => writer.onMessage(connection, ClientMessage.PlaceBet(i)))
      writer.onDisconnected(connection)
      val expected = ("connected" +: (1 to 20).map(i => ClientMessage.PlaceBet(i).toString)) :+ "disconnected"
      (1 to 22).map(_ => next(log)) shouldBe expected

  test("a slow table does not block the network thread that delivers the event"):
    val release = CountDownLatch(1)
    val table = new SessionHandler:
      def onConnected(connection: Connection): Unit                        = ()
      def onMessage(connection: Connection, message: ClientMessage): Unit = release.await(5, TimeUnit.SECONDS)
      def onDisconnected(connection: Connection): Unit                     = ()
    withWriter(table): writer =>
      val before = System.nanoTime()
      writer.onMessage(connection, ClientMessage.Hit)
      writer.onMessage(connection, ClientMessage.Stand)
      val elapsedMillis = (System.nanoTime() - before) / 1_000_000
      release.countDown()
      (elapsedMillis < 1000) shouldBe true

  test("an error while processing an event does not stop the following ones"):
    val processed = LinkedBlockingQueue[ClientMessage]()
    val table = new SessionHandler:
      def onConnected(connection: Connection): Unit = ()
      def onMessage(connection: Connection, message: ClientMessage): Unit =
        if message == ClientMessage.Hit then throw RuntimeException("bug in the table")
        processed.put(message)
      def onDisconnected(connection: Connection): Unit = ()
    withWriter(table): writer =>
      writer.onMessage(connection, ClientMessage.Hit)
      writer.onMessage(connection, ClientMessage.Stand)
      next(processed) shouldBe ClientMessage.Stand

  test("after stop, new events are ignored without errors"):
    val probe = OverlapProbe(workMillis = 0)
    val writer = SingleWriterHandler(probe)
    writer.stop()
    writer.onMessage(connection, ClientMessage.Hit) // must not throw
    probe.processed.poll(200, TimeUnit.MILLISECONDS) shouldBe null
