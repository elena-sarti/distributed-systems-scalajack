package server.replication

import protocol.ClientMessage.*
import protocol.ServerMessage
import server.{Cashier, Connection, TableHandler}
import utils.ModelExports.{Deck, Game, StandardCard, Suit, Value}
import utils.TestExports.*
import java.time.Instant
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.*

class TableTakeoverTest extends AnyFunSuite:

  private final class FakeConnection(val id: String) extends Connection:
    def send(message: ServerMessage): Unit = ()

  /** Remembers who is given their money back, and how much. */
  private final class RecordingCashier extends Cashier:
    val balances = ListBuffer[(String, Double)]()
    def returned: List[String] = balances.map(_._1).toList
    def sitDown(username: String): Either[String, Double] = Right(TableHandler.DefaultBalance)
    def standUp(username: String, balance: Double): Unit = balances += username -> balance

  private def card(value: Value): StandardCard = StandardCard(Suit.Diamonds, value)

  /** The state of the failed primary: Alice and Bob seated, in the phase reached by `moves`. */
  private def stateAfter(moves: (String, protocol.ClientMessage)*): TableSnapshot =
    val deck  = List(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Six, Value.Ten, Value.Four, Value.Two, Value.Five)
    val table = TableHandler(newGame = () => Game(List.empty, Deck.testDeck(deck.map(card)*)), clock = () => Instant.parse("2026-01-01T00:00:00Z"))
    val players = Map("Alice" -> FakeConnection("Alice"), "Bob" -> FakeConnection("Bob"))
    players.foreach((name, connection) => table.onMessage(connection, JoinTable(name)))
    moves.foreach((name, move) => table.onMessage(players(name), move))
    table.snapshot

  test("without a state the table starts empty"):
    val table = TableTakeover.resume(None, RecordingCashier())
    table.freeSeats shouldBe Game.MaxPlayersNum

  test("the players keep their seats while they have time to come back: nobody is given up at the take over"):
    val cashier = RecordingCashier()
    val table   = TableTakeover.resume(Some(stateAfter()), cashier)
    table.onTick()
    cashier.returned shouldBe empty
    table.freeSeats shouldBe Game.MaxPlayersNum - 2

  test("a bet placed before the cards are dealt goes back to the player who does not come back"):
    val cashier = RecordingCashier()
    val table   = TableTakeover.resume(Some(stateAfter("Alice" -> PlaceBet(10))), cashier, returnWindow = 0.seconds) // Bob has not bet yet
    table.onTick() // the time to come back is over
    cashier.balances.toSet shouldBe Set("Alice" -> TableHandler.DefaultBalance, "Bob" -> TableHandler.DefaultBalance)

  test("a hand in progress is settled without waiting for anybody who does not come back, and then the seats are free"):
    val cashier = RecordingCashier()
    val state   = stateAfter("Alice" -> PlaceBet(10), "Bob" -> PlaceBet(20)) // Alice's turn
    val table   = TableTakeover.resume(Some(state), cashier, returnWindow = 0.seconds)
    table.onTick()
    cashier.returned.toSet shouldBe Set("Alice", "Bob")
    table.freeSeats shouldBe Game.MaxPlayersNum

  test("the table goes on from the version of the primary, so the new states follow the old ones"):
    val state = stateAfter()
    val table = TableTakeover.resume(Some(state), RecordingCashier())
    table.snapshot.version should be >= state.version
