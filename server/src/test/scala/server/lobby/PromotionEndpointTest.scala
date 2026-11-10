package server.lobby

import protocol.LobbyApi
import protocol.LobbyApi.{PromotionGranted, PromotionRequest}
import server.auth.TestLobby
import server.auth.TestLobby.{capturedOutput, reason}
import server.{HttpLobbyGateway, PromotionAnswer}
import utils.TestExports.*

class PromotionEndpointTest extends AnyFunSuite with BeforeAndAfterEach:

  private var registry: TableRegistry = _
  private var lobby: TestLobby        = _

  override def beforeEach(): Unit =
    registry = TableRegistry()
    registry.register("table-1", "host", 9091)
    lobby = TestLobby(Map(LobbyApi.PromotionPath -> PromotionEndpoint(PromotionArbiter(registry))))

  override def afterEach(): Unit = lobby.stop()

  private def promote(tableId: String = "table-1", epoch: Long = 5) =
    lobby.post(LobbyApi.PromotionPath, LobbyApi.encode(PromotionRequest(tableId, epoch)))

  test("a granted promotion gets 200 with the epoch, and is written in the log"):
    registry.markStatus("table-1", TableStatus.Unavailable)
    capturedOutput(promote()) should (include("table-1") and include("promoted"))

  test("the answer of a granted promotion carries the table and the epoch"):
    registry.markStatus("table-1", TableStatus.Unavailable)
    val response = promote()
    response.statusCode() shouldBe 200
    LobbyApi.decode[PromotionGranted](response.body()) shouldBe Right(PromotionGranted("table-1", 5))

  test("a promotion while the lobby hears the primary gets 409 with the reason"):
    val response = promote()
    response.statusCode() shouldBe 409
    reason(response) should include("table-1")

  test("an unknown table gets 404, a body that is not a request gets 400, and only POST is accepted"):
    promote(tableId = "ghost").statusCode() shouldBe 404
    lobby.post(LobbyApi.PromotionPath, "not json").statusCode() shouldBe 400
    lobby.get(LobbyApi.PromotionPath).statusCode() shouldBe 405

  test("the gateway of a backup tells granted, denied (with the status) and unreachable"):
    val gateway = HttpLobbyGateway(s"http://localhost:${lobby.port}")
    gateway.requestPromotion("table-1", 5) shouldBe PromotionAnswer.Denied(409)
    registry.markStatus("table-1", TableStatus.Unavailable)
    gateway.requestPromotion("table-1", 5) shouldBe PromotionAnswer.Granted
    gateway.requestPromotion("ghost", 5) shouldBe PromotionAnswer.Denied(404)
    HttpLobbyGateway("http://localhost:1").requestPromotion("table-1", 6) shouldBe PromotionAnswer.Unreachable
