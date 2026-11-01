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

  /** A request that failed, with the reason (HTTP 4xx or 5xx). */
  final case class ApiError(reason: String) derives ReadWriter

  def encode[T: ReadWriter](body: T): String = write(body)

  /** Never throws. The parser's message is not returned because it may quote the body. */
  def decode[T: ReadWriter](json: String): Either[String, T] =
    Try(read[T](json)).toEither.left.map(_ => "Malformed request body")
