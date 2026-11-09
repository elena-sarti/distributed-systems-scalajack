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
    override def readLine: IO[String]                            = IO(synchronized(if input.hasNext then input.next() else throw IllegalStateException("no more input")))
    def readLineWithCharset(charset: Charset): IO[String] = readLine
    def print[A](a: A)(using S: Show[A]): IO[Unit]        = IO(synchronized(output += S.show(a)))
    def println[A](a: A)(using S: Show[A]): IO[Unit]      = IO(synchronized(output += S.show(a)))
    def error[A](a: A)(using S: Show[A]): IO[Unit]        = IO.unit
    def errorln[A](a: A)(using S: Show[A]): IO[Unit]      = IO.unit
    def shown: String = synchronized(output.mkString("\n"))

  private val Nine: utils.ModelExports.StandardCard = utils.ModelExports.StandardCard(utils.ModelExports.Suit.Hearts, utils.ModelExports.Value.Nine)
  private val Session = LoggedIn("alice", "a.b.c", 100.0)
  private val Table1  = TableAddress("table-1", "localhost", 9091)

  /** A lobby with one user, alice, who has `money` fiches: the answers given for the seats (as the real lobby does, she
   * gets none while she is below the minimum bet), and for the location of the table.
   *
   * @param balanceError what the lobby says, instead of the balance, when it cannot tell it.
   */
  private final class FakeLobby(seats: List[SeatAnswer] = List(SeatAnswer.Assigned(Table1)),
                                where: LocateAnswer = LocateAnswer.NotSeated,
                                var money: Double = 100.0,
                                balanceError: Option[String] = None) extends LobbyConnection:
    private var nextSeats = seats
    val registered = ListBuffer[String]()
    /** What the client asked for the balance and for the table, in order. */
    val calls = ListBuffer[String]()
    /** How many of the next requests for the table still find the player sitting: the table has not freed the seat yet. */
    var stillSeated = 0
    def register(username: String, password: String) = IO:
      if username == "alice" && registered.isEmpty then Left("This username is already taken")
      else { registered += username; Right(()) }
    def login(username: String, password: String) = IO:
      if password == "secret-password" then Right(Session.copy(username = username)) else Left("Wrong username or password")
    def assign(token: String) = IO:
      if money < FakeLobby.MinBet then SeatAnswer.Refused(FakeLobby.NeedsMoney)
      else
        val answer = nextSeats.headOption.getOrElse(SeatAnswer.Refused("The server is full"))
        nextSeats = nextSeats.drop(1)
        answer
    def locate(token: String): IO[LocateAnswer] = IO:
      calls += "locate"
      if stillSeated > 0 then
        stillSeated -= 1
        LocateAnswer.Found(Table1)
      else where
    def balance(token: String): IO[Either[String, Double]] = IO:
      calls += "balance"
      balanceError.toLeft(money)
    def deposit(token: String, amount: Double): IO[Either[String, Double]] = IO:
      money += amount
      Right(money)
    def withdraw(token: String, amount: Double): IO[Either[String, Double]] = IO:
      if amount > money then Left(FakeLobby.TooMuch)
      else
        money -= amount
        Right(money)

  private object FakeLobby:
    val MinBet     = 5.0
    val NeedsMoney = "You need at least 5 fiches to sit at a table: make a deposit first"
    val TooMuch    = "Your balance is lower than the amount to withdraw"

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
    OnlineSession(lobby, table.connect, waitStart = 10.millis, seatPoll = 1.millis).run.timeout(10.seconds).unsafeRunSync()

  test("a whole game played only from the client: log in, sit at the table, bet, play, see the results"):
    val table   = FakeTable(oneHand)
    val console = ScriptedConsole("L", "alice", "secret-password", "P", "10", "D", "S", "Q")
    play(console, FakeLobby(), table)
    table.received.toList shouldBe List(JoinTable("alice"), PlaceBet(10), Hit, Stand)
    table.opened.toList shouldBe List(URI.create("ws://localhost:9091"))
    console.shown should include("Hello alice, your balance is 100.0")
    console.shown should include("alice has currently a balance of 110.0 fiches")
    console.shown should include("The game is over!")

  test("wrong credentials are asked again; a new user registers and is logged in at once"):
    val lobby   = FakeLobby()
    val console = ScriptedConsole("L", "bob", "wrong", "R", "bob", "secret-password", "P", "L", "Q")
    play(console, lobby, FakeTable(oneHand))
    lobby.registered.toList shouldBe List("bob")
    console.shown should include("Your account has been created")
    console.shown should include("Wrong username or password")

  test("the player can leave the table instead of betting, and then sit down again"):
    val table   = FakeTable(oneHand)
    val console = ScriptedConsole("L", "alice", "secret-password", "P", "L", "P", "L", "Q")
    play(console, FakeLobby(List(SeatAnswer.Assigned(Table1), SeatAnswer.Assigned(Table1))), table)
    table.received.toList shouldBe List(JoinTable("alice"), LeaveTable, JoinTable("alice"), LeaveTable)

  test("while a new table is starting the client waits and asks again; without seats the player is told why"):
    val console = ScriptedConsole("L", "alice", "secret-password", "P", "L", "P", "Q")
    play(console, FakeLobby(List(SeatAnswer.Starting("table-2"), SeatAnswer.Assigned(Table1))), FakeTable(oneHand))
    console.shown should include("a new one is starting")
    console.shown should include("The server is full") // the second time there is no seat

  test("a new player has no money: the lobby refuses the seat, the player deposits, and then sits down"):
    val lobby   = FakeLobby(money = 0.0)
    val table   = FakeTable(oneHand)
    val console = ScriptedConsole("L", "alice", "secret-password", "P", "D", "50", "P", "L", "Q")
    play(console, lobby, table)
    console.shown should include(FakeLobby.NeedsMoney)                       // the first "P": no seat
    console.shown should include("Done: your balance is now 50.0 fiches")
    lobby.money shouldBe 50.0
    table.opened.size shouldBe 1                                              // only the second "P" reached a table
    table.received.toList shouldBe List(JoinTable("alice"), LeaveTable)

  test("the player can withdraw money in the lobby: a withdrawal of too much is refused, and the amount can be cancelled"):
    val lobby   = FakeLobby()
    val table   = FakeTable(oneHand)
    val console = ScriptedConsole("L", "alice", "secret-password", "W", "500", "W", "C", "W", "40", "Q")
    play(console, lobby, table)
    console.shown should include(FakeLobby.TooMuch)
    console.shown should include("Done: your balance is now 60.0 fiches")
    lobby.money shouldBe 60.0
    table.opened shouldBe empty

  test("an amount that is not a number is asked again"):
    val lobby   = FakeLobby(money = 0.0)
    val console = ScriptedConsole("L", "alice", "secret-password", "D", "lots", "-5", "25", "Q")
    play(console, lobby, FakeTable(oneHand))
    console.shown should include("type a positive number of fiches")
    lobby.money shouldBe 25.0

  test("the lobby shows the balance every time, and is shown again when the player has left the table"):
    val console = ScriptedConsole("L", "alice", "secret-password", "P", "L", "Q")
    play(console, FakeLobby(money = 70.0), FakeTable(oneHand))
    console.shown.split("Your balance is 70.0 fiches").length shouldBe 3 // before sitting down, and after leaving

  test("before going back to the lobby the client waits for the table to free the seat, so the balance is up to date"):
    val lobby = FakeLobby()
    lobby.stillSeated = 2 // the table has not saved the balance yet when the player leaves
    play(ScriptedConsole("L", "alice", "secret-password", "P", "L", "Q"), lobby, FakeTable(oneHand))
    lobby.calls.toList shouldBe List("balance", "locate", "locate", "locate", "balance")

  test("if the lobby cannot tell the balance the player is told why, and the menu still works"):
    val console = ScriptedConsole("L", "alice", "secret-password", "Q")
    play(console, FakeLobby(balanceError = Some("The lobby cannot be reached")), FakeTable(oneHand))
    console.shown should include("The lobby cannot be reached")
    console.shown should include("Your balance is not available now")

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
    val console = ScriptedConsole("L", "alice", "secret-password", "P", "10", "S", "Q")
    given Console[IO] = console
    OnlineSession(lobby, connect, seatPoll = 1.millis).run.timeout(10.seconds).unsafeRunSync()
    opened.toList shouldBe List("ws://localhost:9091", "ws://localhost:9191")
    promoted.received.toList shouldBe List(JoinTable("alice"), Stand)
    console.shown should include("looking for your table")
    console.shown should include("You are back at your seat")
    console.shown should include("The game is over!")

  /** A keyboard where the player types when the test says so. */
  private final class SlowConsole(lines: Queue[IO, String]) extends Console[IO]:
    val output = ListBuffer[String]()
    override def readLine: IO[String]                     = lines.take
    def readLineWithCharset(charset: Charset): IO[String] = readLine
    def print[A](a: A)(using S: Show[A]): IO[Unit]        = IO(synchronized(output += S.show(a)))
    def println[A](a: A)(using S: Show[A]): IO[Unit]      = IO(synchronized(output += S.show(a)))
    def error[A](a: A)(using S: Show[A]): IO[Unit]        = IO.unit
    def errorln[A](a: A)(using S: Show[A]): IO[Unit]      = IO.unit
    def shown: String = synchronized(output.mkString("\n"))

  /** A table driven by the test: it records what the client sends, and sends what the test pushes. */
  private final class PushedTable(events: Queue[IO, ConnectionEvent]):
    val received = ListBuffer[ClientMessage]()
    val connect: TableReconnection.Connect = (_, _) =>
      Resource.pure(new ServerConnection:
        def send(message: ClientMessage): IO[Unit] = IO(synchronized(received += message))
        def receive: IO[ConnectionEvent] = events.take)
    def push(message: ServerMessage): IO[Unit] = events.offer(ConnectionEvent.Received(message))
    def sent: List[ClientMessage] = synchronized(received.toList)

  private def eventually(condition: => Boolean): IO[Unit] =
    IO(condition).flatMap(done => if done then IO.unit else IO.sleep(10.millis) >> eventually(condition)).timeout(5.seconds)

  test("when time is up the question is withdrawn, and an answer typed too late is never sent"):
    val scenario =
      for
        keys    <- Queue.unbounded[IO, String]
        fromTable <- Queue.unbounded[IO, ConnectionEvent]
        console  = SlowConsole(keys)
        table    = PushedTable(fromTable)
        discard  = keys.tryTakeN(None).void
        player  <- { given Console[IO] = console; OnlineSession(FakeLobby(), table.connect, discardTypedAhead = discard).play(Session, Table1).start }
        _       <- eventually(table.sent == List(JoinTable("alice")))
        _       <- table.push(BetRequest(100, 5))
        _       <- eventually(console.shown.contains("Type your bet"))
        _       <- table.push(DecisionTimedOut(protocol.Decision.Bet)) // the player is still thinking
        _       <- eventually(console.shown.contains("Time is up"))
        _       <- keys.offer("10")                                    // too late: nobody asks for it any more, and it is
                                                                       // forgotten when the next question appears
        _       <- IO.sleep(200.millis)
        _       <- IO(table.sent shouldBe List(JoinTable("alice")))
        _       <- table.push(BetRequest(100, 5))                     // the next hand: now the bet counts
        _       <- eventually(console.shown.split("Type your bet").length == 3)
        _       <- keys.offer("15")
        _       <- eventually(table.sent == List(JoinTable("alice"), PlaceBet(15)))
        _       <- table.push(ServerMessage.GameOver(List("alice" -> 100.0)))
        outcome <- player.joinWithNever
      yield outcome
    scenario.timeout(10.seconds).unsafeRunSync() shouldBe TableOutcome.GameOver

  test("what happens at the table is shown while the player is still deciding, with the question repeated"):
    val scenario =
      for
        keys      <- Queue.unbounded[IO, String]
        fromTable <- Queue.unbounded[IO, ConnectionEvent]
        console    = SlowConsole(keys)
        table      = PushedTable(fromTable)
        player    <- { given Console[IO] = console; OnlineSession(FakeLobby(), table.connect).play(Session, Table1).start }
        _         <- table.push(BetRequest(100, 5))
        _         <- eventually(console.shown.contains("Type your bet"))
        _         <- table.push(TableUpdate(view(0)))                  // somebody else bets: the player sees it at once
        _         <- eventually(console.shown.contains("Waiting for your bet"))
        _         <- keys.offer("L")
        outcome   <- player.joinWithNever
      yield (outcome, table.sent)
    scenario.timeout(10.seconds).unsafeRunSync() shouldBe (TableOutcome.LeftTheTable, List(JoinTable("alice"), LeaveTable))

  test("the time left announced by the table is shown under the question"):
    val scenario =
      for
        keys      <- Queue.unbounded[IO, String]
        fromTable <- Queue.unbounded[IO, ConnectionEvent]
        console    = SlowConsole(keys)
        table      = PushedTable(fromTable)
        player    <- { given Console[IO] = console; OnlineSession(FakeLobby(), table.connect).play(Session, Table1).start }
        _         <- table.push(TimeToDecide(30)) >> table.push(BetRequest(100, 5))
        _         <- eventually(console.shown.contains("Type your bet"))
        shown      = console.shown
        _         <- keys.offer("L")
        _         <- player.joinWithNever
      yield shown
    scenario.timeout(10.seconds).unsafeRunSync() should include regex "Type your bet.*\\n(Time left: (30|29) s)"

  /** The table with the dealer showing `visible` cards and holding `hidden` face down. */
  private def dealerShowing(visible: Int, hidden: Int) =
    view(2).copy(dealer = protocol.DealerView(List.fill(visible)(Nine), hidden), currentTurn = None)

  test("every move of the dealer is shown after a pause, as if the dealer were turning the cards"):
    val pause = 300.millis
    val scenario =
      for
        keys      <- Queue.unbounded[IO, String]
        fromTable <- Queue.unbounded[IO, ConnectionEvent]
        console    = SlowConsole(keys)
        table      = PushedTable(fromTable)
        _         <- { given Console[IO] = console; OnlineSession(FakeLobby(), table.connect, dealerPause = pause).play(Session, Table1).start }
        _         <- table.push(TableUpdate(dealerShowing(1, 1)))                // the deal: shown at once
        _         <- eventually(console.shown.contains("SCORE: 18"))
        start     <- IO.monotonic
        _         <- table.push(TableUpdate(dealerShowing(2, 0)))                // the reveal
        _         <- table.push(TableUpdate(dealerShowing(3, 0)))                // a card drawn
        _         <- eventually(console.shown.split("\\[Dealer\\]").length == 4)
        end       <- IO.monotonic
      yield end - start
    scenario.timeout(10.seconds).unsafeRunSync() should be >= (pause * 2)
