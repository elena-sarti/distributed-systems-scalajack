package server

import utils.TestExports.*
import utils.ModelExports.{Deck, Game, StandardCard, Suit, Value}
import protocol.{ServerMessage, TableView}
import protocol.ClientMessage.*
import protocol.ServerMessage.*
import server.TableHandler.*
import java.util.UUID
import scala.collection.mutable.ListBuffer

/** The table logic is tested without network nor threads: it is called directly,
 * as the single writer thread would do, with fake connections that record what they receive. */
class TableHandlerTest extends AnyFunSuite with BeforeAndAfterEach:

  private final class FakeConnection(val id: String) extends Connection:
    val inbox = ListBuffer[ServerMessage]()
    def send(message: ServerMessage): Unit = inbox += message

  var table: TableHandler = _

  override def beforeEach(): Unit =
    table = TableHandler()

  /** A table whose deck deals exactly the given cards, in this order. */
  private def tableWithDeck(cards: StandardCard*): TableHandler =
    TableHandler(newGame = () => Game(List.empty, Deck.testDeck(cards*)))

  /** A new client that asks to sit with the given name. */
  private def join(name: String, at: TableHandler = table): FakeConnection =
    val connection = FakeConnection(UUID.randomUUID().toString) // every client is a different connection
    at.onConnected(connection)
    at.onMessage(connection, JoinTable(name))
    connection

  /** The last table update received by the client. */
  private def lastTable(connection: FakeConnection): TableView =
    connection.inbox.collect { case TableUpdate(view) => view }.last

  private def seatedPlayers(connection: FakeConnection): List[String] =
    lastTable(connection).players.map(_.name)

  test("a player who joins is accepted with the initial balance and sees the table"):
    val alice = join("Alice")
    alice.inbox.head shouldBe JoinAccepted("Alice", DefaultBalance)
    seatedPlayers(alice) shouldBe List("Alice")

  test("every seated player is told when someone joins"):
    val alice = join("Alice")
    join("Bob")
    seatedPlayers(alice) shouldBe List("Alice", "Bob")

  test("a name already used at the table is rejected"):
    join("Alice")
    join("Alice").inbox.toList shouldBe List(JoinRejected(NameTaken))

  test("an empty name or a name with '_' is rejected"):
    join("").inbox.toList shouldBe List(JoinRejected(InvalidName))
    join("Al_ice").inbox.toList shouldBe List(JoinRejected(InvalidName))

  test("when the table is full a new player is rejected"):
    (1 to Game.MaxPlayersNum).foreach(i => join(s"Player$i"))
    join("Late").inbox.toList shouldBe List(JoinRejected(TableFull))

  test("a client cannot join twice"):
    val alice = join("Alice")
    table.onMessage(alice, JoinTable("AliceAgain"))
    alice.inbox.last shouldBe ErrorMessage(AlreadySeated)

  test("a client that has not joined cannot play"):
    val stranger = FakeConnection("stranger")
    table.onMessage(stranger, Hit)
    stranger.inbox.toList shouldBe List(ErrorMessage(JoinFirst))

  test("a player who leaves frees the seat and the others are told"):
    val alice = join("Alice")
    val bob   = join("Bob")
    table.onMessage(bob, LeaveTable)
    seatedPlayers(alice) shouldBe List("Alice")
    join("Bob").inbox.head shouldBe JoinAccepted("Bob", DefaultBalance) // the name is free again

  test("a disconnection frees the seat like a voluntary leave"):
    val alice = join("Alice")
    val bob   = join("Bob")
    table.onDisconnected(bob)
    seatedPlayers(alice) shouldBe List("Alice")

  test("the disconnection of a client that never joined changes nothing"):
    val alice = join("Alice")
    val messagesBefore = alice.inbox.size
    table.onDisconnected(FakeConnection("stranger"))
    alice.inbox.size shouldBe messagesBefore

  test("when the first player sits down a hand starts and the bet is requested"):
    val alice = join("Alice")
    alice.inbox.last shouldBe BetRequest(DefaultBalance, Game.MinGameBet)

  test("a player who sits down while the others are betting is asked to bet too"):
    join("Alice")
    join("Bob").inbox.last shouldBe BetRequest(DefaultBalance, Game.MinGameBet)

  test("a valid bet is taken from the balance and shown to the whole table"):
    val alice = join("Alice")
    val bob   = join("Bob")
    table.onMessage(alice, PlaceBet(10))
    val aliceAsSeenByBob = lastTable(bob).players.find(_.name == "Alice").get
    aliceAsSeenByBob.bet shouldBe Some(10)
    aliceAsSeenByBob.balance shouldBe DefaultBalance - 10

  test("an invalid bet is refused and the player can bet again"):
    val alice = join("Alice")
    table.onMessage(alice, PlaceBet(3))    // not a multiple of the minimum bet
    alice.inbox.last shouldBe ErrorMessage(InvalidBet)
    table.onMessage(alice, PlaceBet(1000)) // more than the balance
    alice.inbox.last shouldBe ErrorMessage(InvalidBet)
    table.onMessage(alice, PlaceBet(10))
    lastTable(alice).players.head.bet shouldBe Some(10)

  test("a player cannot bet twice in the same hand"):
    val alice = join("Alice")
    join("Bob") // keeps the table in the betting phase after Alice's bet
    table.onMessage(alice, PlaceBet(10))
    table.onMessage(alice, PlaceBet(10))
    alice.inbox.last shouldBe ErrorMessage(AlreadyBet)

  test("game actions are refused when it is not the player's turn"):
    val alice = join("Alice")
    table.onMessage(alice, Hit)
    alice.inbox.last shouldBe ErrorMessage(NotYourTurn)

  private val two: StandardCard   = StandardCard(Suit.Hearts, Value.Two)
  private val three: StandardCard = StandardCard(Suit.Clubs, Value.Three)
  private val four: StandardCard  = StandardCard(Suit.Hearts, Value.Four)
  private val queen: StandardCard = StandardCard(Suit.Spades, Value.Queen)

  test("when every seated player has bet the cards are dealt, with the dealer's second card hidden"):
    val dealt = tableWithDeck(two, three, four, queen) // Alice, dealer, Alice, dealer (face down)
    val alice = join("Alice", dealt)
    dealt.onMessage(alice, PlaceBet(10))
    val view = lastTable(alice)
    view.players.head.cards shouldBe List(two, four)
    view.dealer.visibleCards shouldBe List(three)
    view.dealer.hiddenCards shouldBe 1

  test("the deal waits for the bet of every seated player"):
    val alice = join("Alice")
    val bob   = join("Bob")
    table.onMessage(alice, PlaceBet(10))
    lastTable(alice).dealer.visibleCards.isEmpty shouldBe true
    table.onMessage(bob, PlaceBet(10))
    lastTable(alice).dealer.visibleCards.size shouldBe 1

  test("a player who leaves during the betting is not waited for"):
    val alice = join("Alice")
    val bob   = join("Bob")
    table.onMessage(alice, PlaceBet(10))
    table.onMessage(bob, LeaveTable)
    lastTable(alice).dealer.visibleCards.size shouldBe 1

  test("bets are refused once the cards are dealt"):
    val alice = join("Alice")
    table.onMessage(alice, PlaceBet(10))
    table.onMessage(alice, PlaceBet(10))
    alice.inbox.last shouldBe ErrorMessage(NotBettingTime)

  test("if everybody leaves, the hand is dropped and the next player starts a new one"):
    val alice = join("Alice")
    table.onMessage(alice, PlaceBet(10))
    table.onMessage(alice, LeaveTable)
    val bob = join("Bob")
    bob.inbox.last shouldBe BetRequest(DefaultBalance, Game.MinGameBet)
    lastTable(bob).dealer.visibleCards.isEmpty shouldBe true
