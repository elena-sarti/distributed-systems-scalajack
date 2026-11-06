package server.lobby

import protocol.LobbyApi
import protocol.LobbyApi.{StartGranted, StartRequest, TableHeartbeat, TableRegistration}
import server.auth.TestLobby
import server.{HeartbeatResult, HttpLobbyGateway, StartAnswer}
import utils.TestExports.*

/** The lobby as the authority on the epochs of the replicated tables, over HTTP: a node gets its epoch when it starts,
 * and a node whose epoch is old can no longer register nor send heartbeats. */
class EpochFencingTest extends AnyFunSuite with BeforeAndAfterEach:

  private var registry: TableRegistry   = _
  private var arbiter: PromotionArbiter = _
  private var lobby: TestLobby          = _

  override def beforeEach(): Unit =
    registry = TableRegistry()
    arbiter = PromotionArbiter(registry)
    lobby = TestLobby(Map(
      LobbyApi.TableRegisterPath  -> TableRegistrationEndpoint(registry, arbiter),
      LobbyApi.TableHeartbeatPath -> TableHeartbeatEndpoint(registry, arbiter),
      LobbyApi.StartPath          -> PrimaryStartEndpoint(arbiter)))

  override def afterEach(): Unit = lobby.stop()

  private def gateway(epoch: Option[Long]) = HttpLobbyGateway(s"http://localhost:${lobby.port}", epoch = () => epoch)

  /** The backup of table-1 has been promoted with epoch 2. */
  private def replaced(): Unit =
    registry.register("table-1", "primary-host", 9091)
    arbiter.admits("table-1", 1)
    registry.markStatus("table-1", TableStatus.Unavailable)
    arbiter.decide("table-1", 2)

  test("a replicated table that starts gets its epoch"):
    val response = lobby.post(LobbyApi.StartPath, LobbyApi.encode(StartRequest("table-1")))
    response.statusCode() shouldBe 200
    LobbyApi.decode[StartGranted](response.body()) shouldBe Right(StartGranted("table-1", 1))

  test("the gateway of a table that starts tells granted, denied and unreachable"):
    gateway(None).requestStart("table-1") shouldBe StartAnswer.Granted(1)
    registry.register("table-1", "host", 9091) // now it plays
    gateway(None).requestStart("table-1") shouldBe StartAnswer.Denied(409)
    HttpLobbyGateway("http://localhost:1").requestStart("table-1") shouldBe StartAnswer.Unreachable

  test("a start request without a table is refused"):
    lobby.post(LobbyApi.StartPath, LobbyApi.encode(StartRequest(" "))).statusCode() shouldBe 400

  test("a primary that has been replaced gets 409 to its heartbeat, and the registry is not touched"):
    replaced()
    registry.register("table-1", "backup-host", 9191) // the promoted node
    gateway(Some(1)).heartbeat("table-1", 0) shouldBe HeartbeatResult.Superseded
    registry.find("table-1").map(_.freeSeats) shouldBe Some(registry.find("table-1").get.capacity)

  test("a primary that has been replaced cannot register again in place of the promoted node"):
    replaced()
    registry.register("table-1", "backup-host", 9191)
    gateway(Some(1)).register("table-1", "primary-host", 9091) shouldBe false
    registry.find("table-1").map(_.host) shouldBe Some("backup-host")

  test("the promoted node, with the current epoch, is admitted"):
    replaced()
    val promoted = gateway(Some(2))
    promoted.register("table-1", "backup-host", 9191) shouldBe true
    promoted.heartbeat("table-1", 5) shouldBe HeartbeatResult.Acknowledged

  test("a table that is not replicated sends no epoch and is never refused"):
    replaced()
    gateway(None).heartbeat("table-1", 3) shouldBe HeartbeatResult.Acknowledged
    lobby.post(LobbyApi.TableHeartbeatPath, LobbyApi.encode(TableHeartbeat("table-1", 3))).statusCode() shouldBe 200
    lobby.post(LobbyApi.TableRegisterPath, LobbyApi.encode(TableRegistration("table-2", "host", 9092))).statusCode() shouldBe 200
