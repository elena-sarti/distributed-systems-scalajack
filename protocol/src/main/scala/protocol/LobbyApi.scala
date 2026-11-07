package protocol

import upickle.default.{ReadWriter, read, write}
import scala.util.Try

/** The HTTP API of the lobby: plain request-response with JSON bodies.
 *
 * The tables use WebSocket because they push events during a hand; the lobby only answers requests,
 * so HTTP is enough. The types are here, next to the table messages, because the client uses them too.
 */
object LobbyApi:

  /** `POST` a [[RegisterRequest]]: answered by [[Registered]] or [[ApiError]]. */
  val RegisterPath = "/register"

  /** `POST` a [[LoginRequest]]: answered by [[LoggedIn]] or [[ApiError]]. */
  val LoginPath = "/login"

  /** `POST` a [[TableRegistration]] (a table node announcing itself): answered by [[TableAck]] or [[ApiError]]. */
  val TableRegisterPath = "/tables/register"

  /** `POST` a [[TableHeartbeat]] (a table node saying it is alive): answered by [[TableAck]], or by an
   * [[ApiError]] with status 404 if the lobby does not know the table (the node must register again). */
  val TableHeartbeatPath = "/tables/heartbeat"

  /** `POST` a [[BackupHeartbeat]] (the backup of a table saying it is alive): answered by [[TableAck]] or by an
   * [[ApiError]]. The lobby learns about a backup from its first heartbeat; it never receives seats. */
  val BackupHeartbeatPath = "/replicas/heartbeat"

  /** `POST` a [[PromotionRequest]] (a backup asking the lobby to let it take the place of its failed primary):
   * answered by 200 and a [[PromotionGranted]], by 409 and an [[ApiError]] if the lobby does not agree (the primary
   * is not silent for it too, or the epoch is not newer than the last promotion), by 404 if the table is unknown. */
  val PromotionPath = "/tables/promote"

  /** `POST` an [[ExclusionRequest]] (a primary that cannot reach its backup asking to go on without it): answered by
   * 200 and an [[ExclusionGranted]], by 409 and an [[ApiError]] if the epoch is not newer than the current one (this
   * primary has been replaced), by 404 if the table is unknown. */
  val ExclusionPath = "/tables/exclude-backup"

  /** `POST` a [[StartRequest]] (a replicated table node that starts and needs its epoch): answered by 200 and a
   * [[StartGranted]], or by 409 and an [[ApiError]] while another node is playing the table (e.g. its promoted backup). */
  val StartPath = "/tables/start"

  /** `POST` a [[LocateRequest]] (a logged in player who lost the connection with their table, e.g. after a failover, asking
   * where it is now): answered by
   * - 200 and a [[TableAddress]]: connect to that table, directly, and sit down again with the same name: the seat is yours;
   * - 404 and an [[ApiError]]: the player is not seated anywhere (the seat has been given up: ask for a new one with `/assign`);
   * - 503 and an [[ApiError]]: the table is not reachable yet (the backup is taking over): try again in a moment;
   * - 401: invalid token. */
  val LocatePath = "/locate"

  /** `POST` an [[AssignRequest]] (a logged in player asking for a seat): answered by
   * - 200 and a [[TableAddress]]: connect to that table, directly;
   * - 202 and a [[TableStarting]]: no seat yet, a new table is starting, ask again in a few seconds;
   * - 401 (invalid token) or 503 (server full, or a new table could not be started) and an [[ApiError]]. */
  val AssignPath = "/assign"

  // The bodies that hold a password or a token do not show it when printed, so it cannot
  // end up in a log by mistake.

  final case class RegisterRequest(username: String, password: String) derives ReadWriter:
    override def toString: String = s"RegisterRequest($username,***)"

  final case class Registered(username: String) derives ReadWriter

  final case class LoginRequest(username: String, password: String) derives ReadWriter:
    override def toString: String = s"LoginRequest($username,***)"

  /** The token is shown to the tables to sit down; it expires after some hours.
   * The balance is the one saved when the user last left a table. */
  final case class LoggedIn(username: String, token: String, balance: Double) derives ReadWriter:
    override def toString: String = s"LoggedIn($username,***,$balance)"

  /** A table node tells the lobby where clients can reach it: `host` and `wsPort` are the address the
   * clients will use, which is not always the one the node sees (e.g. a port published by Docker).
   * A replicated table also sends its epoch, so that the lobby can refuse a primary that has been replaced. */
  final case class TableRegistration(id: String, host: String, wsPort: Int, epoch: Option[Long] = None) derives ReadWriter

  /** Sent by a table node at every period: it shows the node is alive and tells how many seats are free.
   * The epoch, as in the registration. */
  final case class TableHeartbeat(id: String, freeSeats: Int, epoch: Option[Long] = None) derives ReadWriter

  final case class TableAck(id: String) derives ReadWriter

  /** Sent by the backup node of `tableId` at every period: it shows the backup is alive. */
  final case class BackupHeartbeat(tableId: String) derives ReadWriter

  /** The backup of `tableId` asks to become its primary with `epoch`, greater than the one of the primary it follows. */
  final case class PromotionRequest(tableId: String, epoch: Long) derives ReadWriter

  /** The lobby agrees: from now on `epoch` is the current one for the table. */
  final case class PromotionGranted(tableId: String, epoch: Long) derives ReadWriter

  /** The primary of `tableId` asks to go on without its backup with `epoch`, greater than its own. */
  final case class ExclusionRequest(tableId: String, epoch: Long) derives ReadWriter

  /** The lobby agrees: the backup is out, and `epoch` is the current one for the table. */
  final case class ExclusionGranted(tableId: String, epoch: Long) derives ReadWriter

  /** A replicated table node that starts asks to be the primary of `tableId`. */
  final case class StartRequest(tableId: String) derives ReadWriter

  /** The lobby agrees: the node is the primary of the table with `epoch`. */
  final case class StartGranted(tableId: String, epoch: Long) derives ReadWriter

  /** The token proves that the player is logged in: the same one the table will ask for at the WebSocket handshake. */
  final case class LocateRequest(token: String) derives ReadWriter:
    override def toString: String = "LocateRequest(***)"

  final case class AssignRequest(token: String) derives ReadWriter:
    override def toString: String = "AssignRequest(***)"

  /** Where to play: after this the client talks to the table node, and the lobby is not involved any more. */
  final case class TableAddress(tableId: String, host: String, wsPort: Int) derives ReadWriter

  /** A new table node is being started: ask again shortly. */
  final case class TableStarting(tableId: String) derives ReadWriter

  /** A request that failed, with the reason (HTTP 4xx or 5xx). */
  final case class ApiError(reason: String) derives ReadWriter

  def encode[T: ReadWriter](body: T): String = write(body)

  /** Never throws. The parser's message is not returned because it may quote the body. */
  def decode[T: ReadWriter](json: String): Either[String, T] =
    Try(read[T](json)).toEither.left.map(_ => "Malformed request body")
