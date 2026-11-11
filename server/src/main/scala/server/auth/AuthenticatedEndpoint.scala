package server.auth

import protocol.LobbyApi
import protocol.LobbyApi.ApiError
import upickle.default.ReadWriter

/** An endpoint for the requests of a logged in player. It answers 400 if the body is not valid and 401 if the
 * token is forged or expired, and only then lets the subclass answer for the user. The token is never logged.
 *
 * @tparam R a message that carries a session token.
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
