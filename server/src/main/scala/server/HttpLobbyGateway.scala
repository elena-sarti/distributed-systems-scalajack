package server

import protocol.LobbyApi
import protocol.LobbyApi.{BackupHeartbeat, ExclusionRequest, PromotionRequest, TableHeartbeat, TableRegistration}
import upickle.default.ReadWriter
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration
import scala.util.control.NonFatal

/** The lobby reached over its HTTP API.
 *
 * @param baseUrl the address of the lobby, e.g. `http://lobby:8080`.
 * @param timeout how long to wait to connect and to get an answer: a lobby that does not answer must not
 *                block the table's heartbeat thread for long.
 */
final class HttpLobbyGateway(baseUrl: String, timeout: Duration = Duration.ofSeconds(2)) extends LobbyGateway, PromotionGateway, ExclusionGateway:
  private val http = HttpClient.newBuilder().connectTimeout(timeout).build()

  def register(id: String, host: String, wsPort: Int): Boolean =
    post(LobbyApi.TableRegisterPath, TableRegistration(id, host, wsPort)) match
      case Some(200) => true
      case other =>
        System.err.println(s"The lobby did not accept the registration of table $id: ${other.fold("no answer")(s => s"status $s")}")
        false

  def heartbeat(id: String, freeSeats: Int): HeartbeatResult =
    toResult(post(LobbyApi.TableHeartbeatPath, TableHeartbeat(id, freeSeats)))

  def backupHeartbeat(tableId: String): HeartbeatResult =
    toResult(post(LobbyApi.BackupHeartbeatPath, BackupHeartbeat(tableId)))

  def requestPromotion(tableId: String, epoch: Long): PromotionAnswer =
    toAnswer(post(LobbyApi.PromotionPath, PromotionRequest(tableId, epoch)))

  def requestExclusion(tableId: String, epoch: Long): PromotionAnswer =
    toAnswer(post(LobbyApi.ExclusionPath, ExclusionRequest(tableId, epoch)))

  private def toAnswer(status: Option[Int]): PromotionAnswer = status match
    case Some(200)  => PromotionAnswer.Granted
    case Some(code) => PromotionAnswer.Denied(code)
    case None       => PromotionAnswer.Unreachable

  /** What the status of a heartbeat's answer means for the node (shared by the two kinds of heartbeat). */
  private def toResult(status: Option[Int]): HeartbeatResult = status match
    case Some(200) => HeartbeatResult.Acknowledged
    case Some(404) => HeartbeatResult.UnknownTable
    case _         => HeartbeatResult.Unreachable

  /** The status of the answer, or `None` if there was no answer (connection refused, timeout...). */
  private def post[T: ReadWriter](path: String, body: T): Option[Int] =
    try
      val request = HttpRequest.newBuilder(URI.create(baseUrl.stripSuffix("/") + path))
        .timeout(timeout)
        .header("Content-Type", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(LobbyApi.encode(body)))
        .build()
      Some(http.send(request, HttpResponse.BodyHandlers.discarding()).statusCode())
    catch case NonFatal(_) => None
