package protocol

import utils.TestExports.*
import protocol.LobbyApi.*

class LobbyApiTest extends AnyFunSuite:

  test("a registration request survives a JSON round-trip"):
    val request = RegisterRequest("Alice", "very secret password")
    LobbyApi.decode[RegisterRequest](LobbyApi.encode(request)) shouldBe Right(request)

  test("the answers survive a JSON round-trip"):
    LobbyApi.decode[Registered](LobbyApi.encode(Registered("Alice"))) shouldBe Right(Registered("Alice"))
    LobbyApi.decode[ApiError](LobbyApi.encode(ApiError("Username already taken"))) shouldBe Right(ApiError("Username already taken"))

  test("a registration request is plain JSON with the two fields"):
    LobbyApi.encode(RegisterRequest("Alice", "pw")) shouldBe """{"username":"Alice","password":"pw"}"""

  test("a printed registration request does not show the password"):
    val printed = RegisterRequest("Alice", "very secret password").toString
    printed should include("Alice")
    printed should not include "very secret password"

  test("a malformed body is rejected without throwing and without quoting it"):
    val body = """{"username":"Alice","password":"very secret password""" // not closed
    val result = LobbyApi.decode[RegisterRequest](body)
    result.isLeft shouldBe true
    result.left.toOption.get should not include "very secret password"

  test("login request and answer survive a JSON round-trip"):
    val request = LoginRequest("Alice", "very secret password")
    LobbyApi.decode[LoginRequest](LobbyApi.encode(request)) shouldBe Right(request)
    val answer = LoggedIn("Alice", "a.b.c", 117.5)
    LobbyApi.decode[LoggedIn](LobbyApi.encode(answer)) shouldBe Right(answer)

  test("a printed login request or answer does not show the password nor the token"):
    LoginRequest("Alice", "very secret password").toString should not include "very secret password"
    LoggedIn("Alice", "the-session-token", 100.0).toString should not include "the-session-token"

  test("what a table node sends to the lobby survives a JSON round-trip"):
    val registration = TableRegistration("table-1", "localhost", 9091)
    LobbyApi.decode[TableRegistration](LobbyApi.encode(registration)) shouldBe Right(registration)
    val heartbeat = TableHeartbeat("table-1", 4)
    LobbyApi.decode[TableHeartbeat](LobbyApi.encode(heartbeat)) shouldBe Right(heartbeat)
    LobbyApi.decode[TableAck](LobbyApi.encode(TableAck("table-1"))) shouldBe Right(TableAck("table-1"))

  test("a heartbeat is plain JSON with the id and the free seats"):
    LobbyApi.encode(TableHeartbeat("table-1", 4)) shouldBe """{"id":"table-1","freeSeats":4}"""

  test("the paths of the table nodes are under /tables and different from each other"):
    LobbyApi.TableRegisterPath should startWith("/tables/")
    LobbyApi.TableHeartbeatPath should startWith("/tables/")
    LobbyApi.TableRegisterPath should not equal LobbyApi.TableHeartbeatPath
