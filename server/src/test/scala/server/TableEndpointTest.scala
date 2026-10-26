package server

import utils.TestExports.*
import protocol.{ClientMessage, ServerMessage}
import server.TestSupport.next
import java.net.URI
import java.util.concurrent.TimeUnit

class TableEndpointTest extends AnyFunSuite:

  private def withEndpoint(handler: SessionHandler)(body: TableEndpoint => Unit): Unit =
    val endpoint = TableEndpoint(0, handler) // port 0: a free port chosen by the OS
    endpoint.start()
    try body(endpoint)
    finally endpoint.stop()

  private def connect(endpoint: TableEndpoint): TestWsClient =
    TestWsClient(URI.create(s"ws://localhost:${endpoint.boundPort}"))

  test("a client can connect and the handler is notified"):
    val handler = RecordingHandler()
    withEndpoint(handler): endpoint =>
      val client = connect(endpoint)
      next(handler.connected)
      endpoint.connectionCount shouldBe 1
      client.close()

  test("every connection gets a different id"):
    val handler = RecordingHandler()
    withEndpoint(handler): endpoint =>
      connect(endpoint)
      connect(endpoint)
      Set(next(handler.connected).id, next(handler.connected).id).size shouldBe 2

  test("a valid message is decoded and delivered to the handler with its connection"):
    val handler = RecordingHandler()
    withEndpoint(handler): endpoint =>
      val client     = connect(endpoint)
      val connection = next(handler.connected)
      client.send(ClientMessage.JoinTable("Alice"))
      val (from, message) = next(handler.received)
      message shouldBe ClientMessage.JoinTable("Alice")
      from.id shouldBe connection.id

  test("the server can answer the client that wrote"):
    val handler = RecordingHandler((connection, _) => connection.send(ServerMessage.JoinAccepted("Alice", 100.0)))
    withEndpoint(handler): endpoint =>
      val client = connect(endpoint)
      client.send(ClientMessage.JoinTable("Alice"))
      client.receive() shouldBe ServerMessage.JoinAccepted("Alice", 100.0)

  test("a malformed message gets an ErrorMessage and never reaches the handler"):
    val handler = RecordingHandler()
    withEndpoint(handler): endpoint =>
      val client = connect(endpoint)
      client.sendRaw("this is not json")
      client.receive() match
        case ServerMessage.ErrorMessage(_) => succeed
        case other                         => fail(s"Expected an ErrorMessage, got $other")
      handler.received.poll(300, TimeUnit.MILLISECONDS) shouldBe null

  test("messages of the same client are delivered in order"):
    val handler = RecordingHandler()
    withEndpoint(handler): endpoint =>
      val client = connect(endpoint)
      (1 to 50).foreach(i => client.send(ClientMessage.PlaceBet(i)))
      val delivered = (1 to 50).map(_ => next(handler.received)._2)
      delivered shouldBe (1 to 50).map(i => ClientMessage.PlaceBet(i))

  test("when a client disconnects the handler is notified and the connection is released"):
    val handler = RecordingHandler()
    withEndpoint(handler): endpoint =>
      val client     = connect(endpoint)
      val connection = next(handler.connected)
      client.close()
      next(handler.disconnected).id shouldBe connection.id
      endpoint.connectionCount shouldBe 0

  test("repeated connections and disconnections leave no connection behind"):
    val handler = RecordingHandler()
    withEndpoint(handler): endpoint =>
      (1 to 20).foreach: _ =>
        val client = connect(endpoint)
        next(handler.connected)
        client.close()
        next(handler.disconnected)
      endpoint.connectionCount shouldBe 0

  test("stopping the endpoint closes the connected clients"):
    val handler = RecordingHandler()
    withEndpoint(handler): endpoint =>
      val client = connect(endpoint)
      next(handler.connected)
      endpoint.stop()
      client.awaitClosed() shouldBe true
      next(handler.disconnected)
      endpoint.connectionCount shouldBe 0

  test("starting on a port that is already in use fails"):
    withEndpoint(RecordingHandler()): first =>
      val second = TableEndpoint(first.boundPort, RecordingHandler())
      try intercept[IllegalStateException](second.start())
      finally second.stop()
