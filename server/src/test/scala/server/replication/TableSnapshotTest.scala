package server.replication

import utils.TestExports.*
import utils.ModelExports.{Deck, Game, StandardCard, Suit, Value}
import protocol.ServerMessage
import protocol.ClientMessage.*
import protocol.ServerMessage.*
import server.{Connection, TableHandler}
import upickle.default.{read, write}
import java.time.Instant
import scala.collection.mutable.ListBuffer

/** A table rebuilt from a snapshot must be indistinguishable from the one the snapshot was taken from:
 * same state at once, and the same evolution afterwards, whatever moves the players make. */
class TableSnapshotTest extends AnyFunSuite:

  private final class FakeConnection(val id: String) extends Connection:
    val inbox = ListBuffer[ServerMessage]()
    def send(message: ServerMessage): Unit = inbox += message

  private val now = Instant.parse("2026-01-01T00:00:00Z")

  private def card(value: Value): StandardCard = StandardCard(Suit.Diamonds, value)

  private def tableWithDeck(values: Value*): TableHandler =
    TableHandler(newGame = () => Game(List.empty, Deck.testDeck(values.map(card)*)), clock = () => now)

  private def join(name: String, at: TableHandler): FakeConnection =
    val connection = FakeConnection(s"$name@${System.identityHashCode(at)}")
    at.onConnected(connection)
    at.onMessage(connection, JoinTable(name))
    connection

  /** A second table that takes the state of `original`, with a new connection for each of the given players. */
  private def copyOf(original: TableHandler, players: String*): (TableHandler, Map[String, FakeConnection]) =
    val copy = TableHandler(clock = () => now)
    val connections = players.map(name => name -> FakeConnection(s"$name@copy")).toMap
    copy.restore(original.snapshot, connections.get)
    (copy, connections)

  private def lastTable(connection: FakeConnection) =
    connection.inbox.collect { case TableUpdate(view) => view }.last

  private def results(connection: FakeConnection): List[(String, Double)] =
    connection.inbox.collectFirst { case HandResults(balances) => balances }.get

  /** Alice and Bob have bet 10 each and the cards are dealt (deal order: Alice, Bob, dealer, Alice, Bob,
   * dealer face down, then the cards drawn later). */
  private def aliceAndBobWith(values: Value*): (TableHandler, FakeConnection, FakeConnection) =
    val table = tableWithDeck(values*)
    val alice = join("Alice", table)
    val bob   = join("Bob", table)
    List(alice, bob).foreach(table.onMessage(_, PlaceBet(10)))
    (table, alice, bob)

  private def aliceAndBobDealt() = aliceAndBobWith(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Six,
    Value.Ten, Value.Four, Value.Two, Value.Five)

  test("while the players are betting the copy has the same state as the original"):
    val table = tableWithDeck(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Six, Value.Ten)
    val alice = join("Alice", table)
    join("Bob", table)
    table.onMessage(alice, PlaceBet(10))
    val (copy, _) = copyOf(table, "Alice", "Bob")
    copy.snapshot shouldBe table.snapshot
    copy.freeSeats shouldBe table.freeSeats

  test("during the turns the copy has the same state as the original"):
    val (table, alice, _) = aliceAndBobDealt()
    table.onMessage(alice, Hit)
    val (copy, _) = copyOf(table, "Alice", "Bob")
    copy.snapshot shouldBe table.snapshot

  test("before and after the answers to the insurance the copy has the same state as the original"):
    val (table, alice, bob) = aliceAndBobWith(Value.Ten, Value.Nine, Value.Ace, Value.Six, Value.Seven, Value.Six)
    table.onMessage(alice, AnswerInsurance(true))
    copyOf(table, "Alice", "Bob")._1.snapshot shouldBe table.snapshot
    table.onMessage(bob, AnswerInsurance(false)) // now Alice has paid for it
    copyOf(table, "Alice", "Bob")._1.snapshot shouldBe table.snapshot

  test("the copy goes on with the hand exactly like the original"):
    val (table, alice, bob) = aliceAndBobDealt()
    val (copy, connections) = copyOf(table, "Alice", "Bob")
    val moves = List("Alice" -> Hit, "Alice" -> Stand, "Bob" -> DoubleDown)
    moves.foreach: (player, move) =>
      table.onMessage(if player == "Alice" then alice else bob, move)
      copy.onMessage(connections(player), move)
      copy.snapshot shouldBe table.snapshot
    results(connections("Alice")) shouldBe results(alice)

  test("split hands are rebuilt and the copy plays them like the original"):
    // Alice has a pair of eights against a nine, then come the cards of the split
    val table = tableWithDeck(Value.Eight, Value.Nine, Value.Eight, Value.Seven, Value.Three, Value.Two, Value.Ten)
    val alice = join("Alice", table)
    table.onMessage(alice, PlaceBet(10))
    table.onMessage(alice, Split)
    val (copy, connections) = copyOf(table, "Alice")
    copy.snapshot shouldBe table.snapshot
    List(Hit, Stand, Stand).foreach: move =>
      table.onMessage(alice, move)
      copy.onMessage(connections("Alice"), move)
    copy.snapshot shouldBe table.snapshot
    results(connections("Alice")) shouldBe results(alice)

  test("the snapshot holds the dealer's face-down card, but the clients of the copy still do not see it"):
    val (table, _, _) = aliceAndBobDealt()
    val snapshot = table.snapshot
    snapshot.dealerCards.count(!_.isFaceUp) shouldBe 1
    val (copy, connections) = copyOf(table, "Alice", "Bob")
    copy.onMessage(connections("Alice"), Hit)
    lastTable(connections("Alice")).dealer.hiddenCards shouldBe 1

  test("a player with no connection on the new node keeps the seat and the hand"):
    val (table, _, _) = aliceAndBobDealt()
    val copy = TableHandler(clock = () => now)
    copy.restore(table.snapshot) // nobody has reconnected yet
    copy.freeSeats shouldBe table.freeSeats
    copy.snapshot shouldBe table.snapshot

  test("a snapshot survives the trip to JSON and back"):
    val (table, alice, _) = aliceAndBobWith(Value.Ten, Value.Nine, Value.Ace, Value.Six, Value.Seven, Value.Six)
    table.onMessage(alice, AnswerInsurance(true))
    val snapshot = table.snapshot
    read[TableSnapshot](write(snapshot)) shouldBe snapshot
