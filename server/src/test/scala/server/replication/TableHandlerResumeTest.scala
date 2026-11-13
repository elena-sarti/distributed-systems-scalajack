package server.replication

import protocol.ClientMessage.*
import protocol.ServerMessage
import protocol.ServerMessage.*
import server.{Cashier, Connection, TableHandler}
import utils.ModelExports.{Deck, Game, StandardCard, Suit, Value}
import utils.TestExports.*
import java.time.Instant
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.*

/** After a take over the players come back to their seats and go on from where they were. */
class TableHandlerResumeTest extends AnyFunSuite with BeforeAndAfterEach:

  private val t0 = Instant.parse("2026-01-01T00:00:00Z")

  private final class FakeConnection(val id: String) extends Connection:
    val inbox = ListBuffer[ServerMessage]()
    def send(message: ServerMessage): Unit = inbox += message

  /** Remembers who sits down and who is given what back. */
  private final class RecordingCashier extends Cashier:
    val sitDowns = ListBuffer[String]()
    val balances = ListBuffer[(String, Double)]()
    def returned: Set[String] = balances.map(_._1).toSet
    def sitDown(username: String): Either[String, Double] =
      sitDowns += username
      Right(TableHandler.DefaultBalance)
    def standUp(username: String, balance: Double): Unit = balances += username -> balance

  private var now: Instant            = _
  private var cashier: RecordingCashier = _

  override def beforeEach(): Unit =
    now = t0
    cashier = RecordingCashier()

  private def card(value: Value): StandardCard = StandardCard(Suit.Diamonds, value)

  /** The primary: Alice and Bob seated, in the phase reached by `moves`. Returns its last state, which is what the backup has. */
  private def primaryAfter(moves: (String, protocol.ClientMessage)*): TableSnapshot =
    val deck    = List(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Six, Value.Ten, Value.Four, Value.Two, Value.Five)
    val primary = TableHandler(newGame = () => Game(List.empty, Deck.testDeck(deck.map(card)*)), clock = () => t0)
    val players = Map("Alice" -> FakeConnection("old-Alice"), "Bob" -> FakeConnection("old-Bob"))
    players.foreach((name, connection) => primary.onMessage(connection, JoinTable(name)))
    moves.foreach((name, move) => primary.onMessage(players(name), move))
    primary.snapshot

  /** The backup takes over, `later` after the state was stored. The clock of the new table is the test's. */
  private def takeOver(state: TableSnapshot, window: FiniteDuration = 30.seconds): TableHandler =
    now = t0.plusSeconds(100)
    val table = TableHandler(cashier = cashier, clock = () => now)
    table.restore(state)
    table.awaitReturn(window)
    table

  private var clients = 0 // every connection of the new node has its own id

  private def comesBack(table: TableHandler, name: String): FakeConnection =
    clients += 1
    val client = FakeConnection(s"new-$name-$clients")
    table.onMessage(client, JoinTable(name))
    client

  private def received[A <: ServerMessage](client: FakeConnection)(using tag: scala.reflect.ClassTag[A]): List[A] =
    client.inbox.collect { case message: A => message }.toList

  test("a player who comes back gets the seat, the balance and the table, without sitting down again"):
    val table = takeOver(primaryAfter())
    val alice = comesBack(table, "Alice")
    alice.inbox.head shouldBe JoinAccepted("Alice", TableHandler.DefaultBalance)
    received[TableUpdate](alice) should not be empty
    cashier.sitDowns shouldBe empty // already seated: the cashier is not involved
    table.freeSeats shouldBe Game.MaxPlayersNum - 2

  test("a player who had not bet yet is asked for the bet again, one who had is not"):
    val table = takeOver(primaryAfter("Alice" -> PlaceBet(10))) // Bob has not bet yet
    received[BetRequest](comesBack(table, "Bob")) should have size 1
    received[BetRequest](comesBack(table, "Alice")) shouldBe empty

  test("in the middle of the hand the player whose turn it is gets the same request again, and plays on"):
    val table = takeOver(primaryAfter("Alice" -> PlaceBet(10), "Bob" -> PlaceBet(20))) // Alice's turn
    val alice = comesBack(table, "Alice")
    received[ActionRequest](alice) should have size 1
    table.onMessage(alice, Stand)
    received[ErrorMessage](alice) shouldBe empty // the move is accepted: it was her turn

  test("a player who comes back when it is not their turn is not asked for an action"):
    val table = takeOver(primaryAfter("Alice" -> PlaceBet(10), "Bob" -> PlaceBet(20)))
    received[ActionRequest](comesBack(table, "Bob")) shouldBe empty

  test("the table does not decide for the players before they have had the time to come back"):
    val table = takeOver(primaryAfter("Alice" -> PlaceBet(10), "Bob" -> PlaceBet(20)))
    now = now.plusSeconds(29)
    table.onTick()
    cashier.balances shouldBe empty
    table.freeSeats shouldBe Game.MaxPlayersNum - 2

  test("after the time to come back the players who did not are given up, and the table is free"):
    val table = takeOver(primaryAfter("Alice" -> PlaceBet(10), "Bob" -> PlaceBet(20)))
    now = now.plusSeconds(31)
    table.onTick()
    cashier.returned shouldBe Set("Alice", "Bob")
    table.freeSeats shouldBe Game.MaxPlayersNum

  test("the one who came back keeps the seat while the other is given up"):
    val table = takeOver(primaryAfter("Alice" -> PlaceBet(10), "Bob" -> PlaceBet(20)))
    comesBack(table, "Alice")
    now = now.plusSeconds(31)
    table.onTick()
    cashier.returned should contain("Bob")
    (cashier.returned should not).contain("Alice")
    table.freeSeats shouldBe Game.MaxPlayersNum - 1

  test("before the deal the players who do not come back get their bets back"):
    val table = takeOver(primaryAfter("Alice" -> PlaceBet(10))) // Bob has not bet yet
    now = now.plusSeconds(31)
    table.onTick()
    cashier.balances.toSet shouldBe Set("Alice" -> TableHandler.DefaultBalance, "Bob" -> TableHandler.DefaultBalance)
    table.freeSeats shouldBe Game.MaxPlayersNum

  test("somebody who was not seated before the take over sits down as usual"):
    val table = takeOver(primaryAfter())
    val carol = comesBack(table, "Carol")
    cashier.sitDowns.toList shouldBe List("Carol")
    carol.inbox.head shouldBe JoinAccepted("Carol", TableHandler.DefaultBalance)

  test("a seat can be taken back only once: the second client with the same name is refused"):
    val table = takeOver(primaryAfter())
    comesBack(table, "Alice")
    val impostor = comesBack(table, "Alice")
    impostor.inbox.toList shouldBe List(JoinRejected(TableHandler.NameTaken))

  test("a player who had already lost the seat before the failure (disconnected during the hand) does not get it back"):
    val deck    = List(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Six, Value.Ten, Value.Four, Value.Two, Value.Five)
    val primary = TableHandler(newGame = () => Game(List.empty, Deck.testDeck(deck.map(card)*)), clock = () => t0)
    val alice   = FakeConnection("old-Alice")
    val bob     = FakeConnection("old-Bob")
    List(alice, bob).zip(List("Alice", "Bob")).foreach((connection, name) => primary.onMessage(connection, JoinTable(name)))
    primary.onMessage(alice, PlaceBet(10))
    primary.onMessage(bob, PlaceBet(10))
    primary.onDisconnected(alice) // Alice's client dropped in the middle of the hand: the table plays it, her seat stays until the end
    val table = takeOver(primary.snapshot)
    comesBack(table, "Alice").inbox.toList shouldBe List(JoinRejected(TableHandler.NameTaken))
