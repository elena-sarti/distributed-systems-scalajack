package server.auth

import protocol.LobbyApi
import protocol.LobbyApi.{ApiError, RegisterRequest, Registered}

/** `POST /register`: 201 with the username, 400 if the data is not valid, 409 if the username is taken. */
final class RegistrationEndpoint(registration: RegistrationService) extends JsonPostEndpoint("Registration"):
  import RegistrationEndpoint.*

  protected def answer(body: String): (Int, String) =
    // the parser's error is dropped: it may quote the body, and so the password
    LobbyApi.decode[RegisterRequest](body) match
      case Left(_) => (400, json(ApiError(JsonPostEndpoint.Malformed)))
      case Right(request) => registration.register(request.username, request.password) match
        case Right(user)                              => (201, json(Registered(user.username)))
        case Left(RegistrationError.InvalidUsername) => (400, json(ApiError(InvalidUsername)))
        case Left(RegistrationError.InvalidPassword) => (400, json(ApiError(InvalidPassword)))
        case Left(RegistrationError.UsernameTaken)   => (409, json(ApiError(UsernameTaken)))

object RegistrationEndpoint:
  val InvalidUsername = s"The username must be 1 to ${RegistrationService.MaxUsernameLength} characters, without '_' nor spaces around it"
  val InvalidPassword = s"The password must be ${RegistrationService.MinPasswordLength} to ${RegistrationService.MaxPasswordLength} characters long"
  val UsernameTaken   = "This username is already taken"
