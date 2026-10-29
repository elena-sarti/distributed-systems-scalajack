package server

import utils.TestExports.*
import utils.ModelExports.{Deck, Game, StandardCard, Suit, Value}
import protocol.{MessageCodec, ServerMessage}
import protocol.ClientMessage.{JoinTable, PlaceBet, Stand}
import protocol.ServerMessage.*
import java.net.URI
import java.util.concurrent.CountDownLatch
import scala.collection.mutable.ListBuffer

/** The table as it runs in a node: WebSocket endpoint + single writer + table logic. */
class TableNodeTest extends AnyFunSuite:

  private def withTableNode(body: TableEndpoint => Unit): Unit = withTableNodeOf(TableHandler())(body)

  private def withTableNodeOf(handler: TableHandler)(body: TableEndpoint => Unit): Unit =
    val table    = SingleWriterHandler(handler)
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

  /** Reads the frames received by the client, keeping every one as it travelled on the network,
   * until one of them is the expected message. */
  private def readUntil(client: TestWsClient, frames: ListBuffer[String])(expected: ServerMessage => Boolean): Unit =
    var found = false
    while !found do
      val frame = client.receiveRaw()
      frames += frame
      found = MessageCodec.decodeServerMessage(frame).exists(expected)

  private def decode(frame: String): ServerMessage =
    MessageCodec.decodeServerMessage(frame).getOrElse(fail(s"Not a server message: $frame"))

  test("the dealer's face-down card never travels on the network before the reveal"):
    // deal order: Alice, Bob, dealer, Alice, Bob, dealer (face down). The face-down card is the
    // only club in the deck, so any frame containing "Clubs" would carry it. The dealer has 19 and stands.
    val deck = Deck.testDeck(
      StandardCard(Suit.Hearts, Value.Ten), StandardCard(Suit.Hearts, Value.Nine), StandardCard(Suit.Diamonds, Value.Nine),
      StandardCard(Suit.Hearts, Value.Six), StandardCard(Suit.Diamonds, Value.Seven), StandardCard(Suit.Clubs, Value.King))
    withTableNodeOf(TableHandler(newGame = () => Game(List.empty, deck))): endpoint =>
      val (alice, bob) = (connect(endpoint), connect(endpoint))
      val (aliceFrames, bobFrames) = (ListBuffer[String](), ListBuffer[String]())
      alice.send(JoinTable("Alice"))
      readUntil(alice, aliceFrames)(_.isInstanceOf[BetRequest])
      bob.send(JoinTable("Bob"))
      readUntil(bob, bobFrames)(_.isInstanceOf[BetRequest])
      alice.send(PlaceBet(10))
      bob.send(PlaceBet(10))
      readUntil(alice, aliceFrames)(_.isInstanceOf[ActionRequest])
      alice.send(Stand)
      readUntil(bob, bobFrames)(_.isInstanceOf[ActionRequest])
      bob.send(Stand)
      readUntil(alice, aliceFrames)(_.isInstanceOf[HandResults])
      readUntil(bob, bobFrames)(_.isInstanceOf[HandResults])

      List(aliceFrames, bobFrames).foreach: frames =>
        val (beforeReveal, fromReveal) = frames.toList.span(!_.contains("Clubs"))
        // while the card was face down the clients did get the table, with the card only counted
        val maskedSnapshots = beforeReveal.map(decode).collect:
          case TableUpdate(view) if view.dealer.hiddenCards == 1 => view
        maskedSnapshots.isEmpty shouldBe false
        // the card does travel once revealed, and the first frame with it is the reveal itself
        fromReveal.isEmpty shouldBe false
        decode(fromReveal.head) match
          case TableUpdate(view) => view.dealer.hiddenCards shouldBe 0
          case other             => fail(s"The face-down card travelled in $other")
