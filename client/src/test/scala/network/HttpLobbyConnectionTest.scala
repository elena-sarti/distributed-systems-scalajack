package network

import com.sun.net.httpserver.HttpServer
import protocol.LobbyApi
import protocol.LobbyApi.{AmountRequest, ApiError, BalanceRequest, Funds, LocateRequest, LoggedIn, LoginRequest, RegisterRequest, Registered, TableAddress, TableStarting}
import utils.TestExports.*
import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

class HttpLobbyConnectionTest extends AnyFunSuite:

  /** A lobby that answers `path` with the status and body given, and remembers the body of the request. */
  private def withLobby(path: String, status: Int, body: String = "{}")
                       (test: (HttpLobbyConnection, AtomicReference[String]) => Unit): Unit =
    val request = AtomicReference("")
    val server  = HttpServer.create(InetSocketAddress(0), 0)
    server.createContext(path, exchange =>
      request.set(String(exchange.getRequestBody.readAllBytes(), "UTF-8"))
      val bytes = body.getBytes("UTF-8")
      exchange.sendResponseHeaders(status, bytes.length)
      exchange.getResponseBody.write(bytes)
      exchange.close())
    server.start()
    try test(HttpLobbyConnection(s"http://localhost:${server.getAddress.getPort}"), request)
    finally server.stop(0)

  private def run[A](io: cats.effect.IO[A]): A = io.timeout(10.seconds).unsafeRunSync()

  private val nobody = HttpLobbyConnection("http://localhost:1")

  test("a new user is registered; a refusal comes with the reason of the lobby"):
    withLobby(LobbyApi.RegisterPath, 201, LobbyApi.encode(Registered("alice"))): (lobby, request) =>
      run(lobby.register("alice", "secret-password")) shouldBe Right(())
      LobbyApi.decode[RegisterRequest](request.get) shouldBe Right(RegisterRequest("alice", "secret-password"))
    withLobby(LobbyApi.RegisterPath, 409, LobbyApi.encode(ApiError("This username is already taken"))): (lobby, _) =>
      run(lobby.register("alice", "secret-password")) shouldBe Left("This username is already taken")

  test("a login gives the session, a wrong password the reason"):
    val session = LoggedIn("alice", "a.b.c", 120.0)
    withLobby(LobbyApi.LoginPath, 200, LobbyApi.encode(session)): (lobby, request) =>
      run(lobby.login("alice", "secret-password")) shouldBe Right(session)
      LobbyApi.decode[LoginRequest](request.get) shouldBe Right(LoginRequest("alice", "secret-password"))
    withLobby(LobbyApi.LoginPath, 401, LobbyApi.encode(ApiError("Wrong username or password"))): (lobby, _) =>
      run(lobby.login("alice", "wrong")) shouldBe Left("Wrong username or password")

  test("a seat is assigned, or a table is starting, or there is no seat and the player is told why"):
    val address = TableAddress("table-1", "localhost", 9091)
    withLobby(LobbyApi.AssignPath, 200, LobbyApi.encode(address))((lobby, _) => run(lobby.assign("a.b.c")) shouldBe SeatAnswer.Assigned(address))
    withLobby(LobbyApi.AssignPath, 202, LobbyApi.encode(TableStarting("table-2")))((lobby, _) => run(lobby.assign("a.b.c")) shouldBe SeatAnswer.Starting("table-2"))
    withLobby(LobbyApi.AssignPath, 503, LobbyApi.encode(ApiError("The server is full")))((lobby, _) => run(lobby.assign("a.b.c")) shouldBe SeatAnswer.Refused("The server is full"))

  test("the token is sent to the lobby to locate the table, and the address of the table comes back"):
    val address = TableAddress("table-1", "localhost", 9191)
    withLobby(LobbyApi.LocatePath, 200, LobbyApi.encode(address)): (lobby, request) =>
      run(lobby.locate("a.b.c")) shouldBe LocateAnswer.Found(address)
      LobbyApi.decode[LocateRequest](request.get) shouldBe Right(LocateRequest("a.b.c"))

  test("the refusals to locate the table are told apart"):
    withLobby(LobbyApi.LocatePath, 404)((lobby, _) => run(lobby.locate("a.b.c")) shouldBe LocateAnswer.NotSeated)
    withLobby(LobbyApi.LocatePath, 503)((lobby, _) => run(lobby.locate("a.b.c")) shouldBe LocateAnswer.NotReachableYet)
    withLobby(LobbyApi.LocatePath, 401)((lobby, _) => run(lobby.locate("a.b.c")) shouldBe LocateAnswer.Refused(401))

  test("the balance is asked with the token, and a deposit or a withdrawal gives the new balance"):
    withLobby(LobbyApi.BalancePath, 200, LobbyApi.encode(Funds(50.0))): (lobby, request) =>
      run(lobby.balance("a.b.c")) shouldBe Right(50.0)
      LobbyApi.decode[BalanceRequest](request.get) shouldBe Right(BalanceRequest("a.b.c"))
    withLobby(LobbyApi.DepositPath, 200, LobbyApi.encode(Funds(150.0))): (lobby, request) =>
      run(lobby.deposit("a.b.c", 100.0)) shouldBe Right(150.0)
      LobbyApi.decode[AmountRequest](request.get) shouldBe Right(AmountRequest("a.b.c", 100.0))
    withLobby(LobbyApi.WithdrawPath, 200, LobbyApi.encode(Funds(20.5))): (lobby, request) =>
      run(lobby.withdraw("a.b.c", 30.0)) shouldBe Right(20.5)
      LobbyApi.decode[AmountRequest](request.get) shouldBe Right(AmountRequest("a.b.c", 30.0))

  test("a refusal on the money comes with the reason of the lobby"):
    val reason = "You are sitting at a table: leave it before moving your money"
    withLobby(LobbyApi.DepositPath, 409, LobbyApi.encode(ApiError(reason)))((lobby, _) => run(lobby.deposit("a.b.c", 10.0)) shouldBe Left(reason))
    withLobby(LobbyApi.WithdrawPath, 409, LobbyApi.encode(ApiError(reason)))((lobby, _) => run(lobby.withdraw("a.b.c", 10.0)) shouldBe Left(reason))
    withLobby(LobbyApi.BalancePath, 401, LobbyApi.encode(ApiError("Invalid or expired session token"))): (lobby, _) =>
      run(lobby.balance("a.b.c")) shouldBe Left("Invalid or expired session token")

  test("a lobby that does not answer gives a reason for the player, without errors"):
    run(nobody.register("alice", "secret-password")) shouldBe Left(HttpLobbyConnection.Unreachable)
    run(nobody.login("alice", "secret-password")) shouldBe Left(HttpLobbyConnection.Unreachable)
    run(nobody.assign("a.b.c")) shouldBe SeatAnswer.Refused(HttpLobbyConnection.Unreachable)
    run(nobody.locate("a.b.c")) shouldBe LocateAnswer.Unreachable
    run(nobody.balance("a.b.c")) shouldBe Left(HttpLobbyConnection.Unreachable)
    run(nobody.deposit("a.b.c", 10.0)) shouldBe Left(HttpLobbyConnection.Unreachable)
    run(nobody.withdraw("a.b.c", 10.0)) shouldBe Left(HttpLobbyConnection.Unreachable)
