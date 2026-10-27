package server

import utils.TestExports.*
import protocol.ServerMessage
import protocol.ClientMessage.JoinTable
import protocol.ServerMessage.*
import java.net.URI
import java.util.concurrent.CountDownLatch

/** The table as it runs in a node: WebSocket endpoint + single writer + table logic. */
class TableNodeTest extends AnyFunSuite:

  private def withTableNode(body: TableEndpoint => Unit): Unit =
    val table    = SingleWriterHandler(TableHandler())
    val endpoint = TableEndpoint(0, table) // port 0: a free port chosen by the OS
    endpoint.start()
    try body(endpoint)
    finally
      endpoint.stop()
      table.stop()

  private def connect(endpoint: TableEndpoint): TestWsClient =
    TestWsClient(URI.create(s"ws://localhost:${endpoint.boundPort}"))

  test("a client joins a running table over WebSocket and sees the table"):
    withTableNode: endpoint =>
      val alice = connect(endpoint)
      alice.send(JoinTable("Alice"))
      alice.receive() shouldBe JoinAccepted("Alice", TableHandler.DefaultBalance)
      alice.receive() match
        case TableUpdate(view) => view.players.map(_.name) shouldBe List("Alice")
        case other             => fail(s"Expected a TableUpdate, got $other")

  test("two clients asking for the same name at the same moment: exactly one gets it"):
    withTableNode: endpoint =>
      val clients = List(connect(endpoint), connect(endpoint))
      val start   = CountDownLatch(1)
      val senders = clients.map(client => Thread(() => { start.await(); client.send(JoinTable("Alice")) }))
      senders.foreach(_.start())
      start.countDown() // both requests leave at the same time
      senders.foreach(_.join())
      val answers: List[ServerMessage] = clients.map(_.receive())
      answers.count(_ == JoinAccepted("Alice", TableHandler.DefaultBalance)) shouldBe 1
      answers.count(_ == JoinRejected(TableHandler.NameTaken)) shouldBe 1
