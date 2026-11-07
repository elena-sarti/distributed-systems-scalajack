package protocol

import utils.TestExports.*
import protocol.LobbyApi.*

class LobbyApiTest extends AnyFunSuite:

  test("a registration request survives a JSON round-trip"):
    val request = RegisterRequest("Alice", "very secret password")
    LobbyApi.decode[RegisterRequest](LobbyApi.encode(request)) shouldBe Right(request)

  test("the heartbeat of a backup survives a JSON round-trip"):
    val beat = BackupHeartbeat("table-1")
    LobbyApi.decode[BackupHeartbeat](LobbyApi.encode(beat)) shouldBe Right(beat)

  test("the promotion request and its answer survive a JSON round-trip"):
    val request = PromotionRequest("table-1", 1_700_000_000_001L)
    LobbyApi.decode[PromotionRequest](LobbyApi.encode(request)) shouldBe Right(request)
    val granted = PromotionGranted("table-1", 1_700_000_000_001L)
    LobbyApi.decode[PromotionGranted](LobbyApi.encode(granted)) shouldBe Right(granted)

  test("the start request of a replicated table and its answer survive a JSON round-trip"):
    LobbyApi.decode[StartRequest](LobbyApi.encode(StartRequest("table-1"))) shouldBe Right(StartRequest("table-1"))
    LobbyApi.decode[StartGranted](LobbyApi.encode(StartGranted("table-1", 3))) shouldBe Right(StartGranted("table-1", 3))

  test("a replicated table sends its epoch with registration and heartbeat, the others send nothing more"):
    val registration = TableRegistration("table-1", "localhost", 9091, Some(3))
    LobbyApi.decode[TableRegistration](LobbyApi.encode(registration)) shouldBe Right(registration)
    val heartbeat = TableHeartbeat("table-1", 4, Some(3))
    LobbyApi.decode[TableHeartbeat](LobbyApi.encode(heartbeat)) shouldBe Right(heartbeat)
    LobbyApi.decode[TableHeartbeat]("""{"id":"table-1","freeSeats":4}""") shouldBe Right(TableHeartbeat("table-1", 4))

  test("the exclusion request and its answer survive a JSON round-trip"):
    val request = ExclusionRequest("table-1", 1_700_000_000_001L)
    LobbyApi.decode[ExclusionRequest](LobbyApi.encode(request)) shouldBe Right(request)
    val granted = ExclusionGranted("table-1", 1_700_000_000_001L)
    LobbyApi.decode[ExclusionGranted](LobbyApi.encode(granted)) shouldBe Right(granted)

  test("the locate request survives a JSON round-trip and does not show the token"):
    val request = LocateRequest("a-token")
    LobbyApi.decode[LocateRequest](LobbyApi.encode(request)) shouldBe Right(request)
    request.toString should not include "a-token"

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

  test("an assignment request and its answers survive a JSON round-trip"):
    val request = AssignRequest("a.b.c")
    LobbyApi.decode[AssignRequest](LobbyApi.encode(request)) shouldBe Right(request)
    val address = TableAddress("table-1", "localhost", 9091)
    LobbyApi.decode[TableAddress](LobbyApi.encode(address)) shouldBe Right(address)
    LobbyApi.decode[TableStarting](LobbyApi.encode(TableStarting("table-2"))) shouldBe Right(TableStarting("table-2"))

  test("a printed assignment request does not show the token"):
    AssignRequest("the-session-token").toString should not include "the-session-token"
