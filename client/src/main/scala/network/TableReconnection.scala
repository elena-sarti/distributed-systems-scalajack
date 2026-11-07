package network

import cats.effect.{IO, Resource}
import protocol.ClientMessage.JoinTable
import protocol.LobbyApi.TableAddress
import protocol.ServerMessage.{JoinAccepted, JoinRejected}
import java.net.URI
import scala.concurrent.duration.*

/** The player is back in the seat they had: the connection with the node that plays the table now, and their balance.
 * The table sends right after what it is waiting for from the player, if anything, so the hand goes on. */
final case class Reseated(connection: ServerConnection, balance: Double)

/** The seat cannot be taken back: the lobby says that the player sits nowhere, or the table refuses them (e.g. they came
 * back too late and the seat has been given up). A new seat must be asked for. */
final class SeatLostException(reason: String) extends RuntimeException(reason)

/** The table could not be reached again within the time allowed. */
final class ReconnectionFailedException(reason: String) extends RuntimeException(reason)

/** What a client does when it loses the connection with its table, e.g. because the node failed and the backup took
 * its place: it asks the lobby where the table is now, connects there with the same session token and sits down again
 * with the same name, which gives it back its seat (the client does not need to know that a failover happened).
 *
 * While the table is not reachable (the backup is still taking over, the lobby or the node do not answer yet) it
 * tries again every `retryEvery`, until `giveUpAfter`.
 */
object TableReconnection:
  private val AnswerTimeout = 5.seconds

  /** The connection with the table, as [[WebSocketConnection.connect]] opens it. */
  type Connect = (URI, String) => Resource[IO, ServerConnection]

  def reconnect(lobby: LobbyConnection, token: String, name: String,
                connect: Connect = (uri, token) => WebSocketConnection.connect(uri, Some(token)),
                retryEvery: FiniteDuration = 1.second, giveUpAfter: FiniteDuration = 60.seconds): Resource[IO, Reseated] =

    def attempt(deadline: FiniteDuration): Resource[IO, Reseated] =
      Resource.eval(lobby.locate(token)).flatMap:
        case LocateAnswer.Found(address) =>
          sitAgain(address).attempt.flatMap:
            case Right(reseated)               => Resource.pure(reseated)
            case Left(lost: SeatLostException) => Resource.raiseError[IO, Reseated, Throwable](lost)
            case Left(_)                       => retry(deadline) // the node is not ready yet, or the connection dropped
        case LocateAnswer.NotSeated       => Resource.raiseError[IO, Reseated, Throwable](SeatLostException("You are not seated at any table"))
        case LocateAnswer.Refused(status) => Resource.raiseError[IO, Reseated, Throwable](ReconnectionFailedException(s"The lobby refused the request ($status)"))
        case LocateAnswer.NotReachableYet | LocateAnswer.Unreachable => retry(deadline)

    def retry(deadline: FiniteDuration): Resource[IO, Reseated] =
      Resource.eval(IO.monotonic).flatMap: now =>
        if now >= deadline then Resource.raiseError[IO, Reseated, Throwable](ReconnectionFailedException("The table could not be reached again"))
        else Resource.sleep[IO](retryEvery).flatMap(_ => attempt(deadline))

    def sitAgain(address: TableAddress): Resource[IO, Reseated] =
      connect(URI.create(s"ws://${address.host}:${address.wsPort}"), token).evalMap: connection =>
        connection.send(JoinTable(name)) *> answerTo(connection).timeout(AnswerTimeout)

    Resource.eval(IO.monotonic).flatMap(start => attempt(start + giveUpAfter))

  /** The answer of the table to the request to sit down again. What comes before it is not about this client. */
  private def answerTo(connection: ServerConnection): IO[Reseated] =
    connection.receive.flatMap:
      case ConnectionEvent.Received(JoinAccepted(_, balance)) => IO.pure(Reseated(connection, balance))
      case ConnectionEvent.Received(JoinRejected(reason))     => IO.raiseError(SeatLostException(reason))
      case ConnectionEvent.Disconnected(reason)               => IO.raiseError(ReconnectionFailedException(reason))
      case _                                                  => answerTo(connection)
