package protocol

import upickle.default.{ReadWriter, read, write}
import scala.util.Try

/** The HTTP API of the lobby: plain request-response, with JSON bodies.
 *
 * The table speaks WebSocket because it pushes events to the clients during a hand; the lobby only answers
 * requests (register, and later log in and ask for a table), so HTTP is enough. These types live in the
 * protocol module because the client (G1) uses the same ones to build the requests and read the answers.
 */
object LobbyApi:

  /** Where a new user registers: `POST` with a [[RegisterRequest]] body. */
  val RegisterPath = "/register"

  /** Body of a registration.
   *
   * `toString` hides the password: if the request is ever printed, by mistake or by a log, the password
   * does not end up in the output. */
  final case class RegisterRequest(username: String, password: String) derives ReadWriter:
    override def toString: String = s"RegisterRequest($username,***)"

  /** Answer to a registration that succeeded (HTTP 201). */
  final case class Registered(username: String) derives ReadWriter

  /** Answer to a request that failed, with a human readable reason (HTTP 4xx or 5xx). */
  final case class ApiError(reason: String) derives ReadWriter

  def encode[T: ReadWriter](body: T): String = write(body)

  /** Never throws: the body comes from the network. The reason of the error is not detailed on purpose,
   * because the parser's message may quote the body, and the body may contain a password. */
  def decode[T: ReadWriter](json: String): Either[String, T] =
    Try(read[T](json)).toEither.left.map(_ => "Malformed request body")
