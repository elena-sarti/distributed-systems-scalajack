package controller

import cats.effect.{IO, Ref, Resource}
import cats.effect.std.{Console, Queue}
import cats.Show
import cats.syntax.all.*
import network.*
import protocol.{ClientMessage, ServerMessage}
import protocol.ClientMessage.*
import protocol.LobbyApi.{LoggedIn, TableAddress}
import protocol.ServerMessage.*
import utils.TestExports.*
import java.net.URI
import java.nio.charset.Charset
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.*

class OnlineSessionTest extends AnyFunSuite:

  private final class ScriptedConsole(lines: String*) extends Console[IO]:
    private val input = Iterator(lines*)
    val output        = ListBuffer[String]()
    override def readLine: IO[String]                            = IO(if input.hasNext then input.next() else throw IllegalStateException("no more input"))
    def readLineWithCharset(charset: Charset): IO[String] = readLine
    def print[A](a: A)(using S: Show[A]): IO[Unit]        = IO(output += S.show(a))
    def println[A](a: A)(using S: Show[A]): IO[Unit]      = IO(output += S.show(a))
    def error[A](a: A)(using S: Show[A]): IO[Unit]        = IO.unit
    def errorln[A](a: A)(using S: Show[A]): IO[Unit]      = IO.unit
    def shown: String = output.mkString("\n")

  private val Nine: utils.ModelExports.StandardCard = utils.ModelExports.StandardCard(utils.ModelExports.Suit.Hearts, utils.ModelExports.Value.Nine)
  private val Session = LoggedIn("alice", "a.b.c", 100.0)
  private val Table1  = TableAddress("table-1", "localhost", 9091)

  /** A lobby with one user, alice, and the answers given for the seats and the location of the table. */
  private final class FakeLobby(seats: List[SeatAnswer] = List(SeatAnswer.Assigned(Table1)),
                                where: LocateAnswer = LocateAnswer.Found(Table1)) extends LobbyConnection:
    private var nextSeats = seats
    val registered = ListBuffer[String]()
    def register(username: String, password: String) = IO:
      if username == "alice" && registered.isEmpty then Left("This username is already taken")
      else { registered += username; Right(()) }
    def login(username: String, password: String) = IO:
      if password == "secret-password" then Right(Session.copy(username = username)) else Left("Wrong username or password")
    def assign(token: String) = IO:
      val answer = nextSeats.headOption.getOrElse(SeatAnswer.Refused("The server is full"))
      nextSeats = nextSeats.drop(1)
      answer
    def locate(token: String) = IO.pure(where)

  /** A table that answers each message of the client with the messages of `rule`, and records what it receives.
   * After `dropAfter` messages from the client the connection drops, as when the node fails. */
  private final class FakeTable(rule: PartialFunction[ClientMessage, List[ServerMessage]], dropAfter: Int = Int.MaxValue):
    val received = ListBuffer[ClientMessage]()
    val opened   = ListBuffer[URI]()
    val connect: TableReconnection.Connect = (uri, _) =>
      Resource.eval(Queue.unbounded[IO, ConnectionEvent]).map: events =>
        opened += uri
        new ServerConnection:
          def send(message: ClientMessage): IO[Unit] =
            IO(received += message).flatMap: _ =>
              if received.size == dropAfter then events.offer(ConnectionEvent.Disconnected("the node is gone"))
              else rule.lift(message).getOrElse(Nil).traverse_(m => events.offer(ConnectionEvent.Received(m)))
          def receive: IO[ConnectionEvent] = events.take

  private def view(cards: Int) = protocol.TableView(
    List(protocol.PlayerView("alice", List.fill(cards)(Nine),
      utils.ModelExports.PlayerState.Active, 90, Some(10))),
    protocol.DealerView(Nil, 0), Some("alice"), cards)

  /** A table where alice bets, hits once and stands; the hand ends and with it the game at the table. */
  private val oneHand: PartialFunction[ClientMessage, List[ServerMessage]] =
    case JoinTable(name) => List(JoinAccepted(name, 100), BetRequest(100, 5))
    case PlaceBet(_)     => List(TableUpdate(view(2)), ActionRequest("alice", canDoubleDown = true, canSplit = false))
    case Hit             => List(TableUpdate(view(3)), ActionRequest("alice", canDoubleDown = false, canSplit = false))
    case Stand           => List(HandResults(List("alice" -> 110.0)), ServerMessage.GameOver(List("alice" -> 110.0)))

  private def play(console: ScriptedConsole, lobby: LobbyConnection, table: FakeTable): Unit =
    given Console[IO] = console
    OnlineSession(lobby, table.connect, waitStart = 10.millis).run.timeout(10.seconds).unsafeRunSync()

  test("a whole game played only from the client: log in, sit at the table, bet, play, see the results"):
    val table   = FakeTable(oneHand)
    val console = ScriptedConsole("L", "alice", "secret-password", "10", "D", "S", "N")
    play(console, FakeLobby(), table)
    table.received.toList shouldBe List(JoinTable("alice"), PlaceBet(10), Hit, Stand)
    table.opened.toList shouldBe List(URI.create("ws://localhost:9091"))
    console.shown should include("Hello alice, your balance is 100.0")
    console.shown should include("alice has currently a balance of 110.0 fiches")
    console.shown should include("The game is over!")

  test("wrong credentials are asked again; a new user registers and is logged in at once"):
    val lobby   = FakeLobby()
    val console = ScriptedConsole("L", "bob", "wrong", "R", "bob", "secret-password", "L", "N")
    play(console, lobby, FakeTable(oneHand))
    lobby.registered.toList shouldBe List("bob")
    console.shown should include("Your account has been created")
    console.shown should include("Wrong username or password")

  test("the player can leave the table instead of betting, and then sit down again"):
    val table   = FakeTable(oneHand)
    val console = ScriptedConsole("L", "alice", "secret-password", "L", "Y", "L", "N")
    play(console, FakeLobby(List(SeatAnswer.Assigned(Table1), SeatAnswer.Assigned(Table1))), table)
    table.received.toList shouldBe List(JoinTable("alice"), LeaveTable, JoinTable("alice"), LeaveTable)

  test("while a new table is starting the client waits and asks again; without seats the player is told why"):
    val console = ScriptedConsole("L", "alice", "secret-password", "L", "Y", "N")
    play(console, FakeLobby(List(SeatAnswer.Starting("table-2"), SeatAnswer.Assigned(Table1))), FakeTable(oneHand))
    console.shown should include("a new one is starting")
    console.shown should include("The server is full") // the second time there is no seat

  test("after a failover the client finds its table again and goes on with the hand"):
    // the first connection drops right after the bet; the second one is the promoted node, which gives the seat back
    val table = FakeTable(oneHand.orElse { case _ => Nil }, dropAfter = 2)
    val resumed: PartialFunction[ClientMessage, List[ServerMessage]] =
      case JoinTable(name) => List(JoinAccepted(name, 90), TableUpdate(view(2)), ActionRequest("alice", canDoubleDown = true, canSplit = false))
      case Stand           => List(HandResults(List("alice" -> 110.0)), ServerMessage.GameOver(List("alice" -> 110.0)))
    val promoted = FakeTable(resumed)
    val opened   = ListBuffer[String]()
    val connect: TableReconnection.Connect = (uri, token) =>
      opened += uri.toString
      if opened.size == 1 then table.connect(uri, token) else promoted.connect(uri, token)
    val lobby   = FakeLobby(where = LocateAnswer.Found(TableAddress("table-1", "localhost", 9191)))
    val console = ScriptedConsole("L", "alice", "secret-password", "10", "S", "N")
    given Console[IO] = console
    OnlineSession(lobby, connect).run.timeout(10.seconds).unsafeRunSync()
    opened.toList shouldBe List("ws://localhost:9091", "ws://localhost:9191")
    promoted.received.toList shouldBe List(JoinTable("alice"), Stand)
    console.shown should include("looking for your table")
    console.shown should include("You are back at your seat")
    console.shown should include("The game is over!")
