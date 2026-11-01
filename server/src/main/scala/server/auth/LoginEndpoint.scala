package server.auth

import protocol.LobbyApi
import protocol.LobbyApi.{ApiError, LoggedIn, LoginRequest}

/** `POST /login`: 200 with the session token, 401 if username or password is wrong. */
final class LoginEndpoint(login: LoginService) extends JsonPostEndpoint("Login"):
  import LoginEndpoint.*

  protected def answer(body: String): (Int, String) =
    LobbyApi.decode[LoginRequest](body) match
      case Left(_) => (400, json(ApiError(JsonPostEndpoint.Malformed)))
      case Right(request) => login.login(request.username, request.password) match
        case Some(session) => (200, json(LoggedIn(session.username, session.token, session.balance)))
        case None          => (401, json(ApiError(WrongCredentials)))

object LoginEndpoint:
  // the same answer for an unknown user and for a wrong password
  val WrongCredentials = "Wrong username or password"
