package server.lobby

import protocol.LobbyApi
import protocol.LobbyApi.{AssignRequest, TableAddress, TableStarting}
import server.auth.SessionTokens
import server.auth.TestLobby
import server.auth.TestLobby.{capturedOutput, reason}
import utils.TestExports.*
import java.time.Instant
import scala.collection.mutable.ListBuffer

class TableAssignmentEndpointTest extends AnyFunSuite with BeforeAndAfterEach:

  private final class FakeProvisioner extends TableProvisioner:
    var works   = true
    val started = ListBuffer.empty[String]

    def start(tableId: String): Boolean =
      started += tableId
      works

  private val t0 = Instant.parse("2026-01-01T10:00:00Z")

  private var now: Instant                = _
  private var registry: TableRegistry     = _
  private var provisioner: FakeProvisioner = _
  private var tokens: SessionTokens       = _
  private var lobby: TestLobby            = _

  override def beforeEach(): Unit =
    now = t0
    registry = TableRegistry(capacity = 1) // one seat per table: easy to fill
    provisioner = FakeProvisioner()
    tokens = SessionTokens("a test secret that is long enough for HMAC".getBytes("UTF-8"), clock = () => now)
    val dispatcher = TableDispatcher(registry, provisioner, Seq("table-1", "table-2"), clock = () => now)
    lobby = TestLobby(Map(LobbyApi.AssignPath -> TableAssignmentEndpoint(dispatcher, tokens)))

  override def afterEach(): Unit = lobby.stop()

  private def assign(token: String = tokens.issue("Alice")) =
    lobby.post(LobbyApi.AssignPath, LobbyApi.encode(AssignRequest(token)))

  test("a logged in player with a free seat gets 200 and the address of the table"):
    registry.register("table-1", "localhost", 9091, now)
    val response = assign()
    response.statusCode() shouldBe 200
    LobbyApi.decode[TableAddress](response.body()) shouldBe Right(TableAddress("table-1", "localhost", 9091))

  test("the answer is only the address: the lobby has nothing to do with the moves of the game"):
    registry.register("table-1", "localhost", 9091, now)
    assign().body() shouldBe """{"tableId":"table-1","host":"localhost","wsPort":9091}"""

  test("a missing, forged or expired token gets 401 and no seat is taken"):
    registry.register("table-1", "localhost", 9091, now)
    val expired = tokens.issue("Alice")
    now = t0.plusSeconds(10L * 24 * 3600)
    Seq("not-a-token", "a.b.c", expired).foreach: token =>
      val response = assign(token)
      response.statusCode() shouldBe 401
      reason(response) shouldBe TableAssignmentEndpoint.InvalidToken
    registry.assignableSeats("table-1", now) shouldBe Some(1)

  test("a body that is not an assignment request gets 400"):
    lobby.post(LobbyApi.AssignPath, "not json").statusCode() shouldBe 400

  test("only POST is accepted"):
    lobby.get(LobbyApi.AssignPath).statusCode() shouldBe 405

  test("when no seat is free and a new table can be started, the answer is 202 and the node is started"):
    registry.register("table-1", "localhost", 9091, now)
    assign().statusCode() shouldBe 200 // takes the only seat
    val response = assign()
    response.statusCode() shouldBe 202
    LobbyApi.decode[TableStarting](response.body()) shouldBe Right(TableStarting("table-2"))
    provisioner.started.toList shouldBe List("table-2")

  test("when the tables are at the maximum and full, the answer is 503 'full' and nothing is started"):
    Seq("table-1", "table-2").foreach: id =>
      registry.register(id, "localhost", 9091, now)
      registry.updateFreeSeats(id, 0)
    val response = assign()
    response.statusCode() shouldBe 503
    reason(response) shouldBe TableAssignmentEndpoint.ServerFull
    provisioner.started shouldBe empty

  test("if the new node cannot be started the answer is 503, with a reason different from 'full'"):
    provisioner.works = false
    val response = assign()
    response.statusCode() shouldBe 503
    reason(response) shouldBe TableAssignmentEndpoint.CannotStart
    TableAssignmentEndpoint.CannotStart should not equal TableAssignmentEndpoint.ServerFull

  test("the log tells who was assigned, never the token"):
    registry.register("table-1", "localhost", 9091, now)
    val token = tokens.issue("Alice")
    val log   = capturedOutput(assign(token))
    log should include("Alice")
    log should include("table-1")
    log should not include token
