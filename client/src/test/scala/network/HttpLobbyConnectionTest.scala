package network

import com.sun.net.httpserver.HttpServer
import protocol.LobbyApi
import protocol.LobbyApi.{LocateRequest, TableAddress}
import utils.TestExports.*
import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

class HttpLobbyConnectionTest extends AnyFunSuite:

  /** A lobby that answers `/locate` with the status and body given, and remembers the body of the request. */
  private def withLobby(status: Int, body: String = "{}")(test: (HttpLobbyConnection, AtomicReference[String]) => Unit): Unit =
    val request = AtomicReference("")
    val server  = HttpServer.create(InetSocketAddress(0), 0)
    server.createContext(LobbyApi.LocatePath, exchange =>
      request.set(String(exchange.getRequestBody.readAllBytes(), "UTF-8"))
      val bytes = body.getBytes("UTF-8")
      exchange.sendResponseHeaders(status, bytes.length)
      exchange.getResponseBody.write(bytes)
      exchange.close())
    server.start()
    try test(HttpLobbyConnection(s"http://localhost:${server.getAddress.getPort}"), request)
    finally server.stop(0)

  private def locate(lobby: HttpLobbyConnection) = lobby.locate("a.b.c").timeout(10.seconds).unsafeRunSync()

  test("the token is sent to the lobby, and the address of the table comes back"):
    val address = TableAddress("table-1", "localhost", 9191)
    withLobby(200, LobbyApi.encode(address)): (lobby, request) =>
      locate(lobby) shouldBe LocateAnswer.Found(address)
      LobbyApi.decode[LocateRequest](request.get) shouldBe Right(LocateRequest("a.b.c"))

  test("the refusals of the lobby are told apart"):
    withLobby(404)((lobby, _) => locate(lobby) shouldBe LocateAnswer.NotSeated)
    withLobby(503)((lobby, _) => locate(lobby) shouldBe LocateAnswer.NotReachableYet)
    withLobby(401)((lobby, _) => locate(lobby) shouldBe LocateAnswer.Refused(401))

  test("a lobby that does not answer is unreachable, without errors"):
    locate(HttpLobbyConnection("http://localhost:1")) shouldBe LocateAnswer.Unreachable
