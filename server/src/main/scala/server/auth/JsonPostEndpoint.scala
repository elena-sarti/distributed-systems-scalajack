package server.auth

import com.sun.net.httpserver.{HttpExchange, HttpHandler}
import protocol.LobbyApi
import protocol.LobbyApi.ApiError
import upickle.default.ReadWriter
import scala.util.control.NonFatal

/** An HTTP endpoint that takes a JSON body by POST and answers with a status and a JSON body.
 * It refuses other methods with 405 and large bodies with 413.
 * Errors on the server give 500 and only their type is logged, never the request.
 *
 * @param operation    what the endpoint does, for the log.
 * @param maxBodyBytes the largest body accepted.
 */
abstract class JsonPostEndpoint(operation: String, maxBodyBytes: Int = JsonPostEndpoint.MaxBodyBytes)
  extends HttpHandler:
  import JsonPostEndpoint.*

  /** Status and JSON body of the answer to a request body. */
  protected def answer(body: String): (Int, String)

  protected def json[T: ReadWriter](body: T): String = LobbyApi.encode(body)

  final def handle(exchange: HttpExchange): Unit =
    try
      val (status, body) =
        try reply(exchange)
        catch case NonFatal(error) =>
          System.err.println(s"$operation failed: ${error.getClass.getSimpleName}: ${error.getMessage}")
          (500, json(ApiError(Unavailable)))
      respond(exchange, status, body)
    finally exchange.close()

  private def reply(exchange: HttpExchange): (Int, String) =
    if exchange.getRequestMethod != "POST" then
      exchange.getResponseHeaders.add("Allow", "POST")
      (405, json(ApiError(OnlyPost)))
    else
      // never read whole: at most one byte more than the limit, to know that it was exceeded
      val bytes = exchange.getRequestBody.readNBytes(maxBodyBytes + 1)
      if bytes.length > maxBodyBytes then (413, json(ApiError(TooLarge)))
      else answer(String(bytes, "UTF-8"))

  private def respond(exchange: HttpExchange, status: Int, body: String): Unit =
    val bytes = body.getBytes("UTF-8")
    exchange.getResponseHeaders.add("Content-Type", "application/json")
    exchange.sendResponseHeaders(status, bytes.length)
    exchange.getResponseBody.write(bytes)

object JsonPostEndpoint:
  /** Username and password fit in far less. */
  val MaxBodyBytes = 4096

  val Malformed   = "The body must be a JSON object with username and password"
  val OnlyPost    = "Only POST is allowed"
  val TooLarge    = "The request body is too large"
  val Unavailable = "The service is not available now, try again later"
