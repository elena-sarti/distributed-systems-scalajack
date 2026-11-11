package protocol

import upickle.default.{ReadWriter, read, write}
import scala.util.Try

/** The HTTP API of the lobby, request-response with JSON bodies.
 *
 * The tables use WebSocket because they push events during a hand; the lobby only answers requests,
 * so HTTP is enough.
 */
object LobbyApi:

  /** `POST` a [[RegisterRequest]]: answered by [[Registered]] or [[ApiError]]. */
  val RegisterPath = "/register"

  /** `POST` a [[LoginRequest]]: answered by [[LoggedIn]] or [[ApiError]]. */
  val LoginPath = "/login"

  /** `POST` a [[TableRegistration]]: answered by [[TableAck]] or [[ApiError]]. */
  val TableRegisterPath = "/tables/register"

  /** `POST` a [[TableHeartbeat]]: answered by [[TableAck]], or by an
   * [[ApiError]] with status 404 if the lobby does not know the table. */
  val TableHeartbeatPath = "/tables/heartbeat"

  /** `POST` a [[BackupHeartbeat]]: answered by [[TableAck]] or by an
   * [[ApiError]]. The lobby learns about a backup from its first heartbeat - it never receives seats. */
  val BackupHeartbeatPath = "/replicas/heartbeat"

  /** `POST` a [[PromotionRequest]]:
   * answered by 200 and a [[PromotionGranted]],
   * by 409 and an [[ApiError]] if the lobby does not agree,
   * or by 404 if the table is unknown. */
  val PromotionPath = "/tables/promote"

  /** `POST` an [[ExclusionRequest]]: answered by
   * 200 and an [[ExclusionGranted]],
   * by 409 and an [[ApiError]] if the epoch is not newer than the current one,
   * by 404 if the table is unknown. */
  val ExclusionPath = "/tables/exclude-backup"

  /** `POST` a [[StartRequest]] : answered
   * by 200 and a [[StartGranted]],
   * or by 409 and an [[ApiError]] while another node is playing the table. */
  val StartPath = "/tables/start"

  /** `POST` a [[LocateRequest]]: answered
   * by 200 and a [[TableAddress]],
   * by 404 and an [[ApiError]] if the seat has been given up,
   * by 503 and an [[ApiError]] if the table is not reachable yet,
   * or by 401 if the token is invalid */
  val LocatePath = "/locate"

  /** `POST` an [[AssignRequest]]: answered
   * by 200 and a [[TableAddress]],
   * by 202 and a [[TableStarting]] if a new table is starting so there is no seat yet,
   * by 401 (invalid token), 403 (the balance is below the minimum bet: deposit first) or 503 (server full, or a new
   * table could not be started) and an [[ApiError]]. */
  val AssignPath = "/assign"

  /** `POST` a [[BalanceRequest]]: the balance the player has now. */
  val BalancePath = "/balance"

  /** `POST` an [[AmountRequest]]: adds the amount to the balance. */
  val DepositPath = "/deposit"

  /** `POST` an [[AmountRequest]]: takes the amount from the balance. */
  val WithdrawPath = "/withdraw"

  final case class RegisterRequest(username: String, password: String) derives ReadWriter:
    override def toString: String = s"RegisterRequest($username,***)"

  final case class Registered(username: String) derives ReadWriter

  final case class LoginRequest(username: String, password: String) derives ReadWriter:
    override def toString: String = s"LoginRequest($username,***)"

  /** The token is shown to the tables to sit down and it expires after some hours.
   * The balance is the one saved when the user last left a table. */
  final case class LoggedIn(username: String, token: String, balance: Double) derives ReadWriter:
    override def toString: String = s"LoggedIn($username,***,$balance)"

  /** A table node tells the lobby where clients can reach it: `host` and `wsPort` are the address the
   * clients will use.
   * A replicated table also sends its epoch, so that the lobby can refuse a primary that has been replaced. */
  final case class TableRegistration(id: String, host: String, wsPort: Int, epoch: Option[Long] = None) derives ReadWriter

  /** Sent by a table node at every period: it shows the node is alive and tells how many seats are free and the epoch. */
  final case class TableHeartbeat(id: String, freeSeats: Int, epoch: Option[Long] = None) derives ReadWriter

  final case class TableAck(id: String) derives ReadWriter

  /** Sent by the backup node of `tableId` at every period: it shows the backup is alive. */
  final case class BackupHeartbeat(tableId: String) derives ReadWriter

  /** The backup of tableId asks to become its primary with epoch greater than the one of the primary it follows. */
  final case class PromotionRequest(tableId: String, epoch: Long) derives ReadWriter

  /** The lobby agrees, so from now on epoch is the current one for the table. */
  final case class PromotionGranted(tableId: String, epoch: Long) derives ReadWriter

  /** The primary of tableId asks to go on without its backup with epoch greater than its own. */
  final case class ExclusionRequest(tableId: String, epoch: Long) derives ReadWriter

  /** The lobby agrees so the backup is out, and epoch is the current one for the table. */
  final case class ExclusionGranted(tableId: String, epoch: Long) derives ReadWriter

  /** A replicated table node that starts asks to be the primary of tableId. */
  final case class StartRequest(tableId: String) derives ReadWriter

  /** The lobby agrees so the node is the primary of the table. */
  final case class StartGranted(tableId: String, epoch: Long) derives ReadWriter

  /** The token proves that the player is logged in. */
  final case class LocateRequest(token: String) derives ReadWriter:
    override def toString: String = "LocateRequest(***)"

  final case class AssignRequest(token: String) derives ReadWriter:
    override def toString: String = "AssignRequest(***)"

  /** A logged in player asking for their balance. */
  final case class BalanceRequest(token: String) derives ReadWriter:
    override def toString: String = "BalanceRequest(***)"

  /** A logged in player asking to deposit or to withdraw amount fiches. */
  final case class AmountRequest(token: String, amount: Double) derives ReadWriter:
    override def toString: String = s"AmountRequest(***,$amount)"

  /** The balance of the player after the request. */
  final case class Funds(balance: Double) derives ReadWriter

  /** Where to play, so that the client talks to the table node and the lobby is not involved anymore. */
  final case class TableAddress(tableId: String, host: String, wsPort: Int) derives ReadWriter

  /** A new table node is being started. */
  final case class TableStarting(tableId: String) derives ReadWriter

  /** A request that failed, with the reason. */
  final case class ApiError(reason: String) derives ReadWriter

  def encode[T: ReadWriter](body: T): String = write(body)

  /** The parser's message is not returned because it may quote the body. */
  def decode[T: ReadWriter](json: String): Either[String, T] =
    Try(read[T](json)).toEither.left.map(_ => "Malformed request body")
