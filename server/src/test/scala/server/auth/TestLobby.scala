package server.auth

import com.sun.net.httpserver.HttpHandler
import protocol.LobbyApi
import protocol.LobbyApi.ApiError
import server.{HealthServer, NodeRole}
import java.io.{ByteArrayOutputStream, PrintStream}
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

/** A lobby HTTP server on a free port with the given routes, and a client to call it. */
final class TestLobby(routes: Map[String, HttpHandler]):
  private val server = HealthServer(NodeRole.Lobby, 0, routes)
  server.start()

  def post(path: String, body: String): HttpResponse[String] =
    send(HttpRequest.newBuilder(uri(path)).POST(HttpRequest.BodyPublishers.ofString(body)))

  def get(path: String): HttpResponse[String] = send(HttpRequest.newBuilder(uri(path)).GET())

  def stop(): Unit = server.stop()

  private def uri(path: String) = URI.create(s"http://localhost:${server.boundPort}$path")
  private def send(request: HttpRequest.Builder) = TestLobby.http.send(request.build(), HttpResponse.BodyHandlers.ofString())

object TestLobby:
  private val http = HttpClient.newHttpClient()

  /** The reason in an error answer. */
  def reason(response: HttpResponse[String]): String =
    LobbyApi.decode[ApiError](response.body()).map(_.reason)
      .getOrElse(throw AssertionError(s"Not an error: ${response.body()}"))

  /** Everything written on stdout and stderr, by any thread, while `body` runs. */
  def capturedOutput(body: => Unit): String =
    val captured   = ByteArrayOutputStream()
    val capture    = PrintStream(captured, true, "UTF-8")
    val (out, err) = (System.out, System.err)
    System.setOut(capture)
    System.setErr(capture)
    try body
    finally
      System.setOut(out)
      System.setErr(err)
    captured.toString("UTF-8")
