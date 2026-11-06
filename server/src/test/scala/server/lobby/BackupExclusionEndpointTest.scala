package server.lobby

import protocol.LobbyApi
import protocol.LobbyApi.{ExclusionGranted, ExclusionRequest}
import server.auth.TestLobby
import server.auth.TestLobby.{capturedOutput, reason}
import server.{HttpLobbyGateway, PromotionAnswer}
import utils.TestExports.*

class BackupExclusionEndpointTest extends AnyFunSuite with BeforeAndAfterEach:

  private var registry: TableRegistry = _
  private var arbiter: PromotionArbiter = _
  private var lobby: TestLobby        = _

  override def beforeEach(): Unit =
    registry = TableRegistry()
    registry.register("table-1", "host", 9091)
    arbiter = PromotionArbiter(registry)
    lobby = TestLobby(Map(LobbyApi.ExclusionPath -> BackupExclusionEndpoint(arbiter)))

  override def afterEach(): Unit = lobby.stop()

  private def exclude(tableId: String = "table-1", epoch: Long = 5) =
    lobby.post(LobbyApi.ExclusionPath, LobbyApi.encode(ExclusionRequest(tableId, epoch)))

  test("a granted exclusion gets 200 with the table and the epoch"):
    val response = exclude()
    response.statusCode() shouldBe 200
    LobbyApi.decode[ExclusionGranted](response.body()) shouldBe Right(ExclusionGranted("table-1", 5))

  test("a granted exclusion is written in the log"):
    capturedOutput(exclude()) should include("without the backup")

  test("a primary whose epoch is not newer gets 409 with the reason"):
    registry.markStatus("table-1", TableStatus.Unavailable)
    arbiter.decide("table-1", 5) // its backup has been promoted
    val response = exclude()
    response.statusCode() shouldBe 409
    reason(response) should include("5")

  test("an unknown table gets 404, a body that is not a request gets 400, and only POST is accepted"):
    exclude(tableId = "ghost").statusCode() shouldBe 404
    lobby.post(LobbyApi.ExclusionPath, "not json").statusCode() shouldBe 400
    lobby.get(LobbyApi.ExclusionPath).statusCode() shouldBe 405

  test("the gateway of a primary tells granted, denied (with the status) and unreachable"):
    val gateway = HttpLobbyGateway(s"http://localhost:${lobby.port}")
    gateway.requestExclusion("table-1", 5) shouldBe PromotionAnswer.Granted
    gateway.requestExclusion("table-1", 5) shouldBe PromotionAnswer.Denied(409)
    gateway.requestExclusion("ghost", 6) shouldBe PromotionAnswer.Denied(404)
    HttpLobbyGateway("http://localhost:1").requestExclusion("table-1", 6) shouldBe PromotionAnswer.Unreachable
