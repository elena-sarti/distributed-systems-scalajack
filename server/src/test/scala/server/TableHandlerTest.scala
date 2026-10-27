package server

import utils.TestExports.*
import utils.ModelExports.Game
import protocol.ServerMessage
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

  /** A new client that asks to sit with the given name. */
  private def join(name: String): FakeConnection =
    val connection = FakeConnection(UUID.randomUUID().toString) // every client is a different connection
    table.onConnected(connection)
    table.onMessage(connection, JoinTable(name))
    connection

  /** The players shown in the last table update received by the client. */
  private def seatedPlayers(connection: FakeConnection): List[String] =
    connection.inbox.last match
      case TableUpdate(view) => view.players.map(_.name)
      case other             => fail(s"Expected a TableUpdate, got $other")

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

  test("game actions are refused while no hand is in progress"):
    val alice = join("Alice")
    table.onMessage(alice, Hit)
    alice.inbox.last shouldBe ErrorMessage(NoHandInProgress)

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
