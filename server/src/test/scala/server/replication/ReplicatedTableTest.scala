package server.replication

import utils.TestExports.*
import utils.ModelExports.{Deck, Game, StandardCard, Suit, Value}
import protocol.ServerMessage
import protocol.ClientMessage.*
import protocol.ServerMessage.*
import server.{Cashier, Connection, TableHandler}
import java.time.Instant
import scala.collection.mutable.ListBuffer

/** The primary is tested with the backup in memory: the replicator stores the updates straight into a
 * [[BackupReplica]], and can be made to fail as if the backup could not be reached. */
class ReplicatedTableTest extends AnyFunSuite with BeforeAndAfterEach:

  /** Everything that leaves the primary, in order: messages, balances saved, updates sent to the backup. */
  private var log: ListBuffer[String] = _

  private final class FakeConnection(val id: String) extends Connection:
    val inbox = ListBuffer[ServerMessage]()
    def send(message: ServerMessage): Unit =
      inbox += message
      log += s"$id <- ${message.getClass.getSimpleName}"

  private final class RecordingCashier extends Cashier:
    val returned = ListBuffer[(String, Double)]()
    def sitDown(username: String): Either[String, Double] = Right(TableHandler.DefaultBalance)
    def standUp(username: String, balance: Double): Unit =
      returned += username -> balance
      log += s"saved $username"

  private final class InMemoryReplicator(backup: BackupReplica) extends Replicator:
    var available = true
    var sent = 0
    def replicate(update: ReplicaUpdate): Boolean =
      sent += 1
      log += "replicated"
      available && backup.store(update)

  private var backup: BackupReplica = _
  private var replicator: InMemoryReplicator = _
  private var cashier: RecordingCashier = _
  private var table: TableHandler = _ // the table inside the primary, to compare its state with the backup's
  private var primary: ReplicatedTable = _

  private val now = Instant.parse("2026-01-01T00:00:00Z")

  private def card(value: Value): StandardCard = StandardCard(Suit.Diamonds, value)

  override def beforeEach(): Unit =
    log = ListBuffer()
    backup = BackupReplica()
    replicator = InMemoryReplicator(backup)
    cashier = RecordingCashier()
    val deck = List(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Six, Value.Ten, Value.Four, Value.Two, Value.Five)
    primary = ReplicatedTable(cashier, replicator, epoch = 1, newTable = tableCashier =>
      table = TableHandler(cashier = tableCashier, newGame = () => Game(List.empty, Deck.testDeck(deck.map(card)*)),
        clock = () => now)
      table)

  private def join(name: String): FakeConnection =
    val connection = FakeConnection(name)
    primary.onConnected(connection)
    primary.onMessage(connection, JoinTable(name))
    connection

  private def backupState: TableSnapshot = backup.latest.get.snapshot

  test("after every move the backup has the same state as the primary"):
    val alice = join("Alice")
    backupState shouldBe table.snapshot
    val bob = join("Bob")
    backupState shouldBe table.snapshot
    val moves = List(alice -> PlaceBet(10), bob -> PlaceBet(20), alice -> Hit, alice -> Stand, bob -> Stand)
    moves.foreach: (player, move) =>
      primary.onMessage(player, move)
      backupState shouldBe table.snapshot
    alice.inbox.exists(_.isInstanceOf[HandResults]) shouldBe true // the whole hand has been played

  test("the updates of a primary are numbered in the order they are sent"):
    val alice = join("Alice")
    val first = backup.latest.get.sequence
    primary.onMessage(alice, PlaceBet(10))
    (backup.latest.get.sequence > first) shouldBe true
    backup.latest.get.epoch shouldBe 1

  test("the clients get nothing before the backup has stored the new state"):
    val alice = join("Alice")
    log.toList.take(2) shouldBe List("replicated", "Alice <- JoinAccepted")
    log.clear()
    primary.onMessage(alice, PlaceBet(10))
    log.head shouldBe "replicated"

  test("a balance is saved only after the backup knows that the player has left"):
    val alice = join("Alice")
    log.clear()
    primary.onMessage(alice, LeaveTable)
    log.toList shouldBe List("replicated", "saved Alice")

  test("an event that changes nothing is not sent to the backup"):
    val alice = join("Alice")
    val sent = replicator.sent
    primary.onTick() // no decision is late yet
    primary.onMessage(alice, Hit) // not allowed while betting: only an error for Alice
    replicator.sent shouldBe sent
    alice.inbox.last shouldBe ErrorMessage(TableHandler.NotYourTurn)

  test("if the backup does not store a move the move is undone and the player is asked to try again"):
    val alice = join("Alice")
    val before = table.snapshot
    val received = alice.inbox.size
    replicator.available = false
    primary.onMessage(alice, PlaceBet(10))
    table.snapshot shouldBe before
    alice.inbox.drop(received).toList shouldBe List(ErrorMessage(ReplicatedTable.TablePaused))
    replicator.available = true
    primary.onMessage(alice, PlaceBet(10)) // the same move, once the backup is back
    backupState shouldBe table.snapshot
    alice.inbox.exists(_.isInstanceOf[ActionRequest]) shouldBe true

  test("a player whose sitting down is undone leaves with the money they brought"):
    replicator.available = false
    val alice = join("Alice")
    alice.inbox.toList shouldBe List(ErrorMessage(ReplicatedTable.TablePaused))
    primary.freeSeats shouldBe Game.MaxPlayersNum
    cashier.returned.toList shouldBe List("Alice" -> TableHandler.DefaultBalance)

  test("after an undone move the players still receive what happens at the table"):
    val alice = join("Alice")
    val bob = join("Bob")
    replicator.available = false
    primary.onMessage(alice, PlaceBet(10))
    replicator.available = true
    primary.onMessage(bob, PlaceBet(10))
    lastTableOf(alice) shouldBe lastTableOf(bob)

  test("a backup that follows a newer primary does not acknowledge this one, which then stops"):
    val alice = join("Alice")
    backup.store(ReplicaUpdate(epoch = 2, sequence = 1, table.snapshot)) // another primary has taken over
    val before = table.snapshot
    primary.onMessage(alice, PlaceBet(10))
    table.snapshot shouldBe before
    alice.inbox.last shouldBe ErrorMessage(ReplicatedTable.TablePaused)

  private def lastTableOf(connection: FakeConnection) =
    connection.inbox.collect { case TableUpdate(view) => view }.last

  test("a table that goes on without its backup plays at once and sends nothing more to the backup"):
    val alice = join("Alice")
    replicator.available = false
    primary.goOnAlone(2)
    val sent = replicator.sent
    primary.onMessage(alice, PlaceBet(10))
    replicator.sent shouldBe sent
    alice.inbox.exists(_.isInstanceOf[ActionRequest]) shouldBe true
    primary.currentEpoch shouldBe 2
    primary.isAlone shouldBe true

  test("a balance is saved at once when the table plays alone"):
    val alice = join("Alice")
    replicator.available = false
    primary.goOnAlone(2)
    log.clear()
    primary.onMessage(alice, LeaveTable)
    log.toList shouldBe List("saved Alice")

  test("a table playing alone that takes its backup back sends it the whole state, then waits for it again"):
    val alice = join("Alice")
    replicator.available = false
    primary.goOnAlone(2)
    primary.onMessage(alice, PlaceBet(10)) // played while the backup was out: it does not have it
    replicator.available = true
    primary.takeBackupBack()
    primary.onTick() // the next event, whatever it is
    primary.isAlone shouldBe false
    backup.latest.get.epoch shouldBe 2
    backupState shouldBe table.snapshot
    replicator.available = false
    val before = table.snapshot
    primary.onMessage(alice, Stand) // the moves wait for the backup again
    table.snapshot shouldBe before
    alice.inbox.last shouldBe ErrorMessage(ReplicatedTable.TablePaused)

  test("if the backup does not take the state the table goes on alone"):
    val alice = join("Alice")
    replicator.available = false
    primary.goOnAlone(2)
    primary.takeBackupBack()
    primary.onMessage(alice, PlaceBet(10))
    primary.isAlone shouldBe true
    alice.inbox.exists(_.isInstanceOf[ActionRequest]) shouldBe true

  test("the backup gets the state of a new primary at once, before anybody plays"):
    primary.onTick()
    backup.latest.map(_.epoch) shouldBe Some(1L)
    val sent = replicator.sent
    primary.onTick()
    replicator.sent shouldBe sent // only once

  test("a primary that has been replaced refuses every move, and nothing reaches the clients or the database"):
    val alice = join("Alice")
    primary.stepDown()
    val before   = table.snapshot
    val received = alice.inbox.size
    log.clear()
    primary.onMessage(alice, PlaceBet(10))
    primary.onMessage(alice, LeaveTable)
    primary.onTick()
    table.snapshot shouldBe before
    alice.inbox.drop(received).toList shouldBe List.fill(2)(ErrorMessage(ReplicatedTable.TableMoved))
    log.toList shouldBe List("Alice <- ErrorMessage", "Alice <- ErrorMessage")

  test("a primary that has been replaced writes no balance when it stops"):
    join("Alice")
    primary.stepDown()
    primary.onStop()
    cashier.returned shouldBe empty

  test("a primary that stops while the backup cannot be told writes no balance: the backup still has the players"):
    join("Alice")
    replicator.available = false
    primary.onStop()
    cashier.returned shouldBe empty

  test("a primary that stops gives the players their money once the backup knows it"):
    join("Alice")
    primary.onStop()
    cashier.returned.toList shouldBe List("Alice" -> TableHandler.DefaultBalance)
