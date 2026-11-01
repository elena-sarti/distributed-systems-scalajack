package server.auth

import com.sun.net.httpserver.{HttpExchange, HttpHandler}
import protocol.LobbyApi
import protocol.LobbyApi.{ApiError, RegisterRequest, Registered}
import upickle.default.ReadWriter
import scala.util.Try
import scala.util.control.NonFatal

/** `POST /register` on the lobby: decodes the request, registers the user and answers with a status code
 * and a JSON body.
 *
 *  - 201 Created: the user is registered.
 *  - 400 Bad Request: malformed body, or username / password that break the rules.
 *  - 409 Conflict: the username is already taken.
 *  - 405, 413: wrong method, body too large.
 *  - 500: something went wrong on our side (e.g. the database is down); the client may retry later.
 *
 * Nothing that comes from the client is ever written to the logs: only the kind of an internal error.
 */
final class RegistrationEndpoint(registration: RegistrationService) extends HttpHandler:
  import RegistrationEndpoint.*

  def handle(exchange: HttpExchange): Unit =
    try
      val (status, body) =
        try answer(exchange)
        catch case NonFatal(error) =>
          // the kind of error and its message, which come from our code or the driver: never the request
          System.err.println(s"Registration failed: ${error.getClass.getSimpleName}: ${error.getMessage}")
          (500, json(ApiError(Unavailable)))
      respond(exchange, status, body)
    finally exchange.close()

  private def answer(exchange: HttpExchange): (Int, String) =
    if exchange.getRequestMethod != "POST" then
      exchange.getResponseHeaders.add("Allow", "POST")
      (405, json(ApiError(OnlyPost)))
    else readBody(exchange) match
      case None => (413, json(ApiError(TooLarge)))
      case Some(body) => LobbyApi.decode[RegisterRequest](body) match
        case Left(_)        => (400, json(ApiError(Malformed)))
        case Right(request) => registration.register(request.username, request.password) match
          case Right(user)                              => (201, json(Registered(user.username)))
          case Left(RegistrationError.InvalidUsername) => (400, json(ApiError(InvalidUsername)))
          case Left(RegistrationError.InvalidPassword) => (400, json(ApiError(InvalidPassword)))
          case Left(RegistrationError.UsernameTaken)   => (409, json(ApiError(UsernameTaken)))

  /** The body, or None if it is larger than [[MaxBodyBytes]] (it is never read whole into memory). */
  private def readBody(exchange: HttpExchange): Option[String] =
    val bytes = exchange.getRequestBody.readNBytes(MaxBodyBytes + 1)
    Option.when(bytes.length <= MaxBodyBytes)(String(bytes, "UTF-8"))

  private def respond(exchange: HttpExchange, status: Int, body: String): Unit =
    val bytes = body.getBytes("UTF-8")
    exchange.getResponseHeaders.add("Content-Type", "application/json")
    exchange.sendResponseHeaders(status, bytes.length)
    exchange.getResponseBody.write(bytes)

  private def json[T: ReadWriter](body: T): String = LobbyApi.encode(body)

object RegistrationEndpoint:
  /** Username and password fit in far less: a larger body is refused before parsing it. */
  val MaxBodyBytes = 4096

  val Malformed       = "The body must be a JSON object with username and password"
  val InvalidUsername = s"The username must be 1 to ${RegistrationService.MaxUsernameLength} characters, without '_' nor spaces around it"
  val InvalidPassword = s"The password must be ${RegistrationService.MinPasswordLength} to ${RegistrationService.MaxPasswordLength} characters long"
  val UsernameTaken   = "This username is already taken"
  val OnlyPost        = "Only POST is allowed"
  val TooLarge        = "The request body is too large"
  val Unavailable     = "Registration is not available now, try again later"
