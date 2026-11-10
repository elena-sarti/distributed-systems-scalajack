package server

import protocol.LobbyApi
import protocol.LobbyApi.{BackupHeartbeat, ExclusionRequest, PromotionRequest, StartGranted, StartRequest, TableHeartbeat, TableRegistration}
import upickle.default.ReadWriter
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration
import scala.util.control.NonFatal

/** The lobby reached over its HTTP API.
 *
 * @param baseUrl the address of the lobby.
 * @param timeout how long to wait to connect and to get an answer, so that a silent lobby does not block the heartbeat
 *                thread for long.
 * @param epoch   the epoch of the table, sent with its registration and heartbeats and read at every call since it
 *                grows. None for a table that is not replicated.
 */
final class HttpLobbyGateway(baseUrl: String, timeout: Duration = Duration.ofSeconds(2), epoch: () => Option[Long] = () => None)
  extends LobbyGateway, PromotionGateway, ExclusionGateway, StartGateway:
  private val http = HttpClient.newBuilder().connectTimeout(timeout).build()

  def register(id: String, host: String, wsPort: Int): Boolean =
    post(LobbyApi.TableRegisterPath, TableRegistration(id, host, wsPort, epoch())) match
      case Some(200) => true
      case other =>
        System.err.println(s"The lobby did not accept the registration of table $id: ${other.fold("no answer")(s => s"status $s")}")
        false

  def heartbeat(id: String, freeSeats: Int): HeartbeatResult =
    toResult(post(LobbyApi.TableHeartbeatPath, TableHeartbeat(id, freeSeats, epoch())))

  def backupHeartbeat(tableId: String): HeartbeatResult =
    toResult(post(LobbyApi.BackupHeartbeatPath, BackupHeartbeat(tableId)))

  def requestPromotion(tableId: String, epoch: Long): PromotionAnswer =
    toAnswer(post(LobbyApi.PromotionPath, PromotionRequest(tableId, epoch)))

  def requestExclusion(tableId: String, epoch: Long): PromotionAnswer =
    toAnswer(post(LobbyApi.ExclusionPath, ExclusionRequest(tableId, epoch)))

  def requestStart(tableId: String): StartAnswer =
    send(LobbyApi.StartPath, StartRequest(tableId)) match
      case Some((200, body)) => LobbyApi.decode[StartGranted](body).fold(_ => StartAnswer.Denied(200), granted => StartAnswer.Granted(granted.epoch))
      case Some((code, _))   => StartAnswer.Denied(code)
      case None              => StartAnswer.Unreachable

  private def toAnswer(status: Option[Int]): PromotionAnswer = status match
    case Some(200)  => PromotionAnswer.Granted
    case Some(code) => PromotionAnswer.Denied(code)
    case None       => PromotionAnswer.Unreachable

  /** What the status of the answer to a heartbeat means for the node, for both kinds of heartbeat. */
  private def toResult(status: Option[Int]): HeartbeatResult = status match
    case Some(200) => HeartbeatResult.Acknowledged
    case Some(404) => HeartbeatResult.UnknownTable
    case Some(409) => HeartbeatResult.Superseded
    case _         => HeartbeatResult.Unreachable

  /** The status of the answer, or None if there was none. */
  private def post[T: ReadWriter](path: String, body: T): Option[Int] = send(path, body).map(_._1)

  /** Status and body of the answer, or None if there was no answer. */
  private def send[T: ReadWriter](path: String, body: T): Option[(Int, String)] =
    try
      val request = HttpRequest.newBuilder(URI.create(baseUrl.stripSuffix("/") + path))
        .timeout(timeout)
        .header("Content-Type", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(LobbyApi.encode(body)))
        .build()
      val response = http.send(request, HttpResponse.BodyHandlers.ofString())
      Some((response.statusCode(), response.body()))
    catch case NonFatal(_) => None
