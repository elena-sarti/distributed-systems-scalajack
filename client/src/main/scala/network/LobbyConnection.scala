package network

import cats.effect.IO
import protocol.LobbyApi
import protocol.LobbyApi.{LocateRequest, TableAddress}
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

/** The client's view of the lobby. */
trait LobbyConnection:
  /** Where the table of the player with this session token is now. Never fails: problems are answers. */
  def locate(token: String): IO[LocateAnswer]

/** The lobby reached over its HTTP API, with the JDK's client.
 *
 * @param baseUrl the address of the lobby, e.g. `http://localhost:9080`. */
final class HttpLobbyConnection(baseUrl: String) extends LobbyConnection:
  private val http    = HttpClient.newBuilder().connectTimeout(HttpLobbyConnection.Timeout).build()
  private val locator = URI.create(baseUrl.stripSuffix("/") + LobbyApi.LocatePath)

  def locate(token: String): IO[LocateAnswer] =
    val request = HttpRequest.newBuilder(locator).timeout(HttpLobbyConnection.Timeout)
      .header("Content-Type", "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(LobbyApi.encode(LocateRequest(token))))
      .build()
    IO.fromCompletableFuture(IO(http.sendAsync(request, HttpResponse.BodyHandlers.ofString())))
      .map(response => answer(response.statusCode(), response.body()))
      .handleError(_ => LocateAnswer.Unreachable)

  private def answer(status: Int, body: String): LocateAnswer = status match
    case 200 => LobbyApi.decode[TableAddress](body).fold(_ => LocateAnswer.Refused(status), LocateAnswer.Found(_))
    case 404 => LocateAnswer.NotSeated
    case 503 => LocateAnswer.NotReachableYet
    case _   => LocateAnswer.Refused(status)

object HttpLobbyConnection:
  private val Timeout = JDuration.ofSeconds(5)
