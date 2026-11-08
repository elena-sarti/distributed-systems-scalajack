package controller

import cats.effect.{ExitCode, Resource}
import network.*
import protocol.{ClientMessage, ServerMessage}
import protocol.ClientMessage.*
import protocol.LobbyApi.{LoggedIn, TableAddress}
import protocol.ServerMessage.*
import utils.GameUIExports.*
import view.OnlineView
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
    given Console[IO] = Console.make[IO]
    OnlineView.welcome(lobbyUrl) >> OnlineSession(HttpLobbyConnection(lobbyUrl)).run.as(ExitCode.Success)

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
 * @param connect   opens the connection with a table (a parameter for the tests).
 * @param waitStart how long to wait before asking again for a seat while a new table is starting.
 */
final class OnlineSession(lobby: LobbyConnection,
                          connect: TableReconnection.Connect = (uri, token) => WebSocketConnection.connect(uri, Some(token)),
                          waitStart: FiniteDuration = 3.seconds)(using Console[IO]):

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

  /** Reacts to what the table sends, until the game at the table is over for the player. */
  private def follow(session: LoggedIn, connection: ServerConnection): IO[TableOutcome] =
    val me = session.username
    def next: IO[TableOutcome] = follow(session, connection)
    connection.receive.flatMap:
      case ConnectionEvent.Received(message) => message match
        case JoinAccepted(_, balance)   => OnlineView.info(s"You are at the table with $balance fiches.\n") >> next
        case JoinRejected(reason)       => OnlineView.info(reason).as(TableOutcome.Refused)
        case TableUpdate(table)         => OnlineView.renderTable(table, me) >> next
        case BetRequest(balance, min)   =>
          OnlineView.askBet(balance, min).flatMap:
            case BetChoice.Bet(amount) => connection.send(PlaceBet(amount)) >> next
            case BetChoice.Leave       => connection.send(LeaveTable).as(TableOutcome.LeftTheTable)
        case InsuranceOffer             => OnlineView.askInsurance.flatMap(accept => connection.send(AnswerInsurance(accept))) >> next
        case ActionRequest(hand, dd, s) => chooseAction(hand, dd, s).flatMap(action => connection.send(toMessage(action))) >> next
        case HandResults(balances)      => OnlineView.renderResults(balances) >> next
        case ServerMessage.GameOver(balances) => OnlineView.renderGameOver(balances).as(TableOutcome.GameOver)
        case ErrorMessage(reason)       => OnlineView.info(s"The table says: $reason") >> next
        case DecisionTimedOut(decision) => OnlineView.renderTimeUp(decision) >> next
      case ConnectionEvent.Invalid(_)          => next // a message this client does not understand: nothing to show
      case ConnectionEvent.Disconnected(_)     => comeBack(session)

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
