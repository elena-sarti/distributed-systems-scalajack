package server.auth

import protocol.LobbyApi
import protocol.LobbyApi.ApiError
import upickle.default.ReadWriter

/** A [[JsonPostEndpoint]] for the requests of a logged in player.
 *
 * It does what every such endpoint needs before anything else: decodes the body as `R` (400 if it is not one),
 * checks the session token inside it (401 if it is forged or expired) and only then lets the subclass answer, on
 * behalf of the user the token belongs to. The token is never written in the log.
 *
 * @tparam R the request: any message of [[LobbyApi]] that carries a session token.
 */
abstract class AuthenticatedEndpoint[R: ReadWriter](operation: String, tokens: SessionTokens)
  extends JsonPostEndpoint(operation):

  /** The session token carried by the request. */
  protected def tokenOf(request: R): String

  /** Status and JSON body of the answer to a user who is logged in. */
  protected def answerFor(username: String, request: R): (Int, String)

  protected final def answer(body: String): (Int, String) =
    LobbyApi.decode[R](body) match
      case Left(message)  => (400, json(ApiError(message)))
      case Right(request) => tokens.verify(tokenOf(request)) match
        case None           => (401, json(ApiError(AuthenticatedEndpoint.InvalidToken)))
        case Some(username) => answerFor(username, request)

object AuthenticatedEndpoint:
  val InvalidToken = "Invalid or expired session token"
