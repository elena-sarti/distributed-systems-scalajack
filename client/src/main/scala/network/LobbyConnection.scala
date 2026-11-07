package network

import cats.effect.IO
import protocol.LobbyApi
import protocol.LobbyApi.{ApiError, AssignRequest, LocateRequest, LoggedIn, LoginRequest, RegisterRequest, TableAddress, TableStarting}
import upickle.default.ReadWriter
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration as JDuration

/** What the lobby says about the table of a player who lost the connection with it. */
enum LocateAnswer:
  /** The table is played at this address now (by the node it had before, or by the backup that took its place). */
  case Found(address: TableAddress)
  /** The player sits at no table any more: the seat has been given up. */
  case NotSeated
  /** The table is not reachable yet, e.g. its backup is still taking over: ask again in a moment. */
  case NotReachableYet
  /** The lobby refuses the request (e.g. an expired token, 401): asking again does not help. */
  case Refused(status: Int)
  /** No answer from the lobby. */
  case Unreachable

/** What the lobby says to a player who asks for a seat. */
enum SeatAnswer:
  /** A seat is kept for the player at this table: connect there. */
  case Assigned(address: TableAddress)
  /** All the tables are full and a new one is starting: ask again in a few seconds. */
  case Starting(tableId: String)
  /** No seat now (e.g. the server is full), or the lobby cannot be reached: the reason, for the player. */
  case Refused(reason: String)

/** The client's view of the lobby. Nothing here fails: the problems are answers, with a reason for the player. */
trait LobbyConnection:
  /** Creates a new user. */
  def register(username: String, password: String): IO[Either[String, Unit]]

  /** The session of the user: the token to show to the tables, and the balance. */
  def login(username: String, password: String): IO[Either[String, LoggedIn]]

  /** A seat at a table with room for the player. */
  def assign(token: String): IO[SeatAnswer]

  /** Where the table of the player with this session token is now. */
  def locate(token: String): IO[LocateAnswer]

/** The lobby reached over its HTTP API, with the JDK's client.
 *
 * @param baseUrl the address of the lobby, e.g. `http://localhost:9080`. */
final class HttpLobbyConnection(baseUrl: String) extends LobbyConnection:
  import HttpLobbyConnection.*

  private val http = HttpClient.newBuilder().connectTimeout(Timeout).build()

  def register(username: String, password: String): IO[Either[String, Unit]] =
    post(LobbyApi.RegisterPath, RegisterRequest(username, password)).map:
      case Some((201, _))      => Right(())
      case Some((_, body))     => Left(reasonIn(body))
      case None                => Left(Unreachable)

  def login(username: String, password: String): IO[Either[String, LoggedIn]] =
    post(LobbyApi.LoginPath, LoginRequest(username, password)).map:
      case Some((200, body))   => LobbyApi.decode[LoggedIn](body)
      case Some((_, body))     => Left(reasonIn(body))
      case None                => Left(Unreachable)

  def assign(token: String): IO[SeatAnswer] =
    post(LobbyApi.AssignPath, AssignRequest(token)).map:
      case Some((200, body)) => LobbyApi.decode[TableAddress](body).fold(SeatAnswer.Refused(_), SeatAnswer.Assigned(_))
      case Some((202, body)) => LobbyApi.decode[TableStarting](body).fold(SeatAnswer.Refused(_), s => SeatAnswer.Starting(s.tableId))
      case Some((_, body))   => SeatAnswer.Refused(reasonIn(body))
      case None              => SeatAnswer.Refused(Unreachable)

  def locate(token: String): IO[LocateAnswer] =
    post(LobbyApi.LocatePath, LocateRequest(token)).map:
      case Some((200, body))   => LobbyApi.decode[TableAddress](body).fold(_ => LocateAnswer.Refused(200), LocateAnswer.Found(_))
      case Some((404, _))      => LocateAnswer.NotSeated
      case Some((503, _))      => LocateAnswer.NotReachableYet
      case Some((status, _))   => LocateAnswer.Refused(status)
      case None                => LocateAnswer.Unreachable

  /** Status and body of the answer, or `None` if the lobby did not answer. */
  private def post[T: ReadWriter](path: String, body: T): IO[Option[(Int, String)]] =
    val request = HttpRequest.newBuilder(URI.create(baseUrl.stripSuffix("/") + path)).timeout(Timeout)
      .header("Content-Type", "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(LobbyApi.encode(body)))
      .build()
    IO.fromCompletableFuture(IO(http.sendAsync(request, HttpResponse.BodyHandlers.ofString())))
      .map(response => Option((response.statusCode(), response.body())))
      .handleError(_ => None)

object HttpLobbyConnection:
  private val Timeout = JDuration.ofSeconds(5)

  val Unreachable = "The lobby cannot be reached: check that the server is running, then try again"

  /** The reason the lobby gives in its error, or a generic one. */
  private def reasonIn(body: String): String =
    LobbyApi.decode[ApiError](body).fold(_ => "The lobby refused the request", _.reason)
