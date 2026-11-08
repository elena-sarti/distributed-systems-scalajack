package controller

import cats.effect.{ExitCode, Fiber, Resource}
import cats.effect.std.Queue
import network.*
import protocol.{ClientMessage, ServerMessage}
import protocol.ClientMessage.*
import protocol.LobbyApi.{LoggedIn, TableAddress}
import protocol.ServerMessage.*
import utils.GameUIExports.*
import view.{OnlineView, TerminalConsole}
import view.OnlineView.{Account, BetChoice}
import java.net.URI
import scala.concurrent.duration.*

/** The client of the online game: `java -jar scalajack-client.jar [lobby address]` (by default the lobby of the Docker
 * deployment on this machine, or the one in `SCALAJACK_LOBBY`). The local game of the first version is
 * [[GameController]]. */
object OnlineGame extends IOApp:
  val DefaultLobby = "http://localhost:9080"

  def run(args: List[String]): IO[ExitCode] =
    val lobbyUrl = args.headOption.orElse(sys.env.get("SCALAJACK_LOBBY")).getOrElse(DefaultLobby)
    TerminalConsole.resource.use: terminal =>
      given Console[IO] = terminal
      OnlineView.welcome(lobbyUrl) >>
        OnlineSession(HttpLobbyConnection(lobbyUrl), discardTypedAhead = terminal.discardTypedAhead,
          liveCountdown = System.console() != null).run.as(ExitCode.Success)

/** How a game at a table ended for the player. */
enum TableOutcome:
  case LeftTheTable, GameOver, Refused, ConnectionLost

/** A player of the online game: logs in, asks the lobby for a seat, plays at the table until the game there is over or
 * they leave, and then may sit down again.
 *
 * The table leads the game: the client shows what the table sends and answers what it asks (a bet, the insurance, an
 * action), using the same prompts as the local game. If the connection with the table is lost (e.g. its node failed)
 * the client finds the table again through the lobby and sits down in the same seat ([[TableReconnection]]).
 *
 * The table does not wait for anybody: while the player thinks, the client keeps showing what happens at the table,
 * and if the table decides for the player (time is up) or asks something else, the question on the screen is
 * withdrawn and an answer typed too late is never sent. Under each question the client counts down the time left.
 *
 * @param connect           opens the connection with a table (a parameter for the tests).
 * @param waitStart         how long to wait before asking again for a seat while a new table is starting.
 * @param discardTypedAhead forgets what the player typed before a new question appeared.
 * @param liveCountdown     whether the time left is rewritten every second (only on a real terminal).
 */
final class OnlineSession(lobby: LobbyConnection,
                          connect: TableReconnection.Connect = (uri, token) => WebSocketConnection.connect(uri, Some(token)),
                          waitStart: FiniteDuration = 3.seconds,
                          discardTypedAhead: IO[Unit] = IO.unit,
                          liveCountdown: Boolean = false)(using Console[IO]):
  import OnlineSession.*

  def run: IO[Unit] = signIn.flatMap(playUntilDone)

  /** Logs in, after registering if the player asks for it, until the lobby accepts the credentials. */
  def signIn: IO[LoggedIn] =
    OnlineView.askCredentials.flatMap: credentials =>
      val registered = credentials.account match
        case Account.Register => lobby.register(credentials.username, credentials.password)
          .flatTap(result => IO.whenA(result.isRight)(OnlineView.info("Your account has been created.\n")))
        case Account.Login    => IO.pure(Right(()))
      registered.flatMap:
        case Left(reason) => OnlineView.info(reason) >> signIn
        case Right(_) =>
          lobby.login(credentials.username, credentials.password).flatMap:
            case Left(reason)   => OnlineView.info(reason) >> signIn
            case Right(session) => OnlineView.info(s"Hello ${session.username}, your balance is ${session.balance} fiches.\n").as(session)

  private def playUntilDone(session: LoggedIn): IO[Unit] =
    seat(session).flatMap(_.traverse_(play(session, _))) >>
      OnlineView.askToPlayAgain.flatMap(again => IO.whenA(again)(playUntilDone(session)))

  /** The table the lobby assigns, waiting while a new table starts; `None` if there is no seat now. */
  private def seat(session: LoggedIn): IO[Option[TableAddress]] =
    lobby.assign(session.token).flatMap:
      case SeatAnswer.Assigned(address) => IO.pure(Some(address))
      case SeatAnswer.Starting(_) =>
        OnlineView.info("All the tables are full: a new one is starting, please wait...") >> IO.sleep(waitStart) >> seat(session)
      case SeatAnswer.Refused(reason) => OnlineView.info(reason).as(None)

  /** Sits down at the table and plays there. */
  def play(session: LoggedIn, address: TableAddress): IO[TableOutcome] =
    connect(URI.create(s"ws://${address.host}:${address.wsPort}"), session.token).use: connection =>
      connection.send(JoinTable(session.username)) >> follow(session, connection)

  /** Reacts to what the table sends, until the game at the table is over for the player. What the table sends and what
   * the player answers arrive in the same queue, in the order they happen: the questions are asked in fibers of their
   * own, so the table is never left unheard while the player is typing. */
  private def follow(session: LoggedIn, connection: ServerConnection): IO[TableOutcome] =
    Queue.unbounded[IO, Event].flatMap: events =>
      listen(connection, events).background.surround(react(session, connection, events, State()))

  /** Everything the table sends goes into the queue, up to the end of the connection. */
  private def listen(connection: ServerConnection, events: Queue[IO, Event]): IO[Unit] =
    connection.receive.flatMap: event =>
      events.offer(Event.FromTable(event)) >> (event match
        case ConnectionEvent.Disconnected(_) => IO.unit
        case _                               => listen(connection, events))

  private def react(session: LoggedIn, connection: ServerConnection, events: Queue[IO, Event], state: State): IO[TableOutcome] =
    val me     = session.username
    val asking = state.asking
    def next(stillAsking: Option[Question] = asking): IO[TableOutcome] =
      react(session, connection, events, state.copy(asking = stillAsking))
    /** Shows a new question, in place of the one on the screen if there is one, with the time left under it. */
    def ask(question: String => IO[Answer], reminder: String): IO[TableOutcome] =
      val id = state.nextQuestion
      for
        _       <- withdraw(asking) >> discardTypedAhead
        left    <- state.deadline.traverse(secondsUntil)
        footer   = left.fold("")(seconds => "\n" + OnlineView.timeLeft(seconds))
        counting = state.deadline.filter(_ => liveCountdown).fold(question(footer))(countDown(_).background.surround(question(footer)))
        fiber   <- counting.flatMap(answer => events.offer(Event.Answered(id, answer))).start
        outcome <- react(session, connection, events, State(Some(Question(id, fiber, reminder, state.deadline)), id + 1, None))
      yield outcome

    events.take.flatMap:
      case Event.Answered(id, answer) if asking.exists(_.id == id) => answer match
        case Answer.Send(message) => connection.send(message) >> next(None)
        case Answer.Leave         => connection.send(LeaveTable).as(TableOutcome.LeftTheTable)
      case Event.Answered(_, _) => next() // the answer to a question that was withdrawn: too late, it is not sent
      case Event.FromTable(ConnectionEvent.Received(message)) => message match
        case BetRequest(balance, min) =>
          ask(footer => OnlineView.askBet(balance, min, footer).map:
            case BetChoice.Bet(amount) => Answer.Send(PlaceBet(amount))
            case BetChoice.Leave       => Answer.Leave
          , OnlineView.betReminder(min))
        case InsuranceOffer =>
          ask(footer => OnlineView.askInsurance(footer).map(accept => Answer.Send(AnswerInsurance(accept))), OnlineView.insuranceReminder)
        case ActionRequest(hand, dd, s) =>
          ask(footer => chooseAction(hand, dd, s, footer).map(action => Answer.Send(toMessage(action))), OnlineView.actionReminder(dd, s))
        case TimeToDecide(seconds) =>
          IO.monotonic.flatMap(now => react(session, connection, events, state.copy(deadline = Some(now + seconds.seconds))))
        case DecisionTimedOut(decision) => withdraw(asking) >> OnlineView.renderTimeUp(decision) >> next(None)
        case JoinRejected(reason)       => withdraw(asking) >> OnlineView.info(reason).as(TableOutcome.Refused)
        case ServerMessage.GameOver(balances) =>
          withdraw(asking) >> OnlineView.renderGameOver(balances).as(TableOutcome.GameOver)
        case JoinAccepted(_, balance)   => OnlineView.info(s"You are at the table with $balance fiches.\n") >> next()
        case TableUpdate(table)         => OnlineView.renderTable(table, me) >> asking.traverse_(remind) >> next()
        case HandResults(balances)      => OnlineView.renderResults(balances) >> next()
        case ErrorMessage(reason)       => OnlineView.info(s"The table says: $reason") >> next()
      case Event.FromTable(ConnectionEvent.Invalid(_))      => next() // a message this client does not understand: nothing to show
      case Event.FromTable(ConnectionEvent.Disconnected(_)) => withdraw(asking) >> comeBack(session)

  private def withdraw(asking: Option[Question]): IO[Unit] = asking.traverse_(_.fiber.cancel)

  /** The question again, short, under the table just shown: with the time left on its own line, which the countdown
   * rewrites from now on. */
  private def remind(question: Question): IO[Unit] =
    question.deadline.traverse(secondsUntil).flatMap: left =>
      OnlineView.info(question.reminder + left.fold("")(seconds => "\n" + OnlineView.timeLeft(seconds)))

  /** Every second, the time left until `deadline` (a time of the monotonic clock), down to zero. */
  private def countDown(deadline: FiniteDuration): IO[Unit] =
    IO.monotonic.flatMap: now =>
      val millisLeft = (deadline - now).toMillis
      val untilNextSecond = if millisLeft <= 0 then 0L else (millisLeft - 1) % 1000 + 1
      IO.whenA(millisLeft > 0):
        IO.sleep(untilNextSecond.millis) >> secondsUntil(deadline).flatMap(OnlineView.refreshTimeLeft) >> countDown(deadline)

  private def secondsUntil(deadline: FiniteDuration): IO[Long] =
    IO.monotonic.map(now => math.max(0L, ((deadline - now).toMillis + 999) / 1000))

  /** The table is lost: like after a failover, the client asks the lobby where it is now and sits down again. */
  private def comeBack(session: LoggedIn): IO[TableOutcome] =
    OnlineView.info("The connection with the table is lost: looking for your table...") >>
      TableReconnection.reconnect(lobby, session.token, session.username, connect)
        .use(reseated => OnlineView.info("You are back at your seat.\n") >> follow(session, reseated.connection))
        .handleErrorWith(error => OnlineView.info(error.getMessage).as(TableOutcome.ConnectionLost))

  private def toMessage(action: PlayerAction): ClientMessage = action match
    case PlayerAction.DrawCard   => Hit
    case PlayerAction.Stand      => Stand
    case PlayerAction.DoubleDown => DoubleDown
    case PlayerAction.Split      => Split

private object OnlineSession:
  /** What the player decided: a message for the table, or to leave it. */
  enum Answer:
    case Send(message: ClientMessage)
    case Leave

  /** What happened: the table sent something, or the player answered the question `id`. */
  enum Event:
    case FromTable(event: ConnectionEvent)
    case Answered(id: Int, answer: Answer)

  /** The question on the screen, asked by `fiber`; `reminder` repeats it when the table is shown again, `deadline`
   * is when the table stops waiting for the answer. */
  final case class Question(id: Int, fiber: Fiber[IO, Throwable, Unit], reminder: String, deadline: Option[FiniteDuration])

  /** The question on the screen, the id of the next one, and the time to answer announced for it by the table. */
  final case class State(asking: Option[Question] = None, nextQuestion: Int = 0, deadline: Option[FiniteDuration] = None)
