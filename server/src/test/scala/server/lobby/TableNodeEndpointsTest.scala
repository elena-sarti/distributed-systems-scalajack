package server.lobby

import protocol.LobbyApi
import protocol.LobbyApi.{TableAck, TableHeartbeat, TableRegistration}
import server.auth.TestLobby
import server.auth.TestLobby.{capturedOutput, reason}
import utils.ModelExports.Game
import utils.TestExports.*

class TableNodeEndpointsTest extends AnyFunSuite with BeforeAndAfterEach:

  var registry: TableRegistry = _
  var lobby: TestLobby        = _

  override def beforeEach(): Unit =
    registry = TableRegistry()
    lobby = TestLobby(Map(
      LobbyApi.TableRegisterPath  -> TableRegistrationEndpoint(registry),
      LobbyApi.TableHeartbeatPath -> TableHeartbeatEndpoint(registry)
    ))

  override def afterEach(): Unit = lobby.stop()

  private def register(id: String = "table-1", host: String = "localhost", port: Int = 9091) =
    lobby.post(LobbyApi.TableRegisterPath, LobbyApi.encode(TableRegistration(id, host, port)))

  private def heartbeat(id: String, freeSeats: Int) =
    lobby.post(LobbyApi.TableHeartbeatPath, LobbyApi.encode(TableHeartbeat(id, freeSeats)))

  test("a table that registers gets 200 and appears in the registry, available and empty"):
    val response = register()
    response.statusCode() shouldBe 200
    LobbyApi.decode[TableAck](response.body()) shouldBe Right(TableAck("table-1"))
    registry.find("table-1") shouldBe Some(
      TableInfo("table-1", "localhost", 9091, Game.MaxPlayersNum, Game.MaxPlayersNum, TableStatus.Available))

  test("a registration with a blank id or a wrong port gets 400 with the reason, and registers nothing"):
    Seq(register(id = " "), register(port = 0)).foreach: response =>
      response.statusCode() shouldBe 400
      reason(response) should not be empty
    registry.tables shouldBe empty

  test("a body that is not a registration gets 400"):
    lobby.post(LobbyApi.TableRegisterPath, "not json").statusCode() shouldBe 400

  test("only POST is accepted"):
    lobby.get(LobbyApi.TableRegisterPath).statusCode() shouldBe 405
    lobby.get(LobbyApi.TableHeartbeatPath).statusCode() shouldBe 405

  test("the registration of a table is written in the log"):
    capturedOutput(register()) should include("table-1")

  test("a heartbeat gets 200 and updates the free seats of the table"):
    register()
    val response = heartbeat("table-1", 3)
    response.statusCode() shouldBe 200
    LobbyApi.decode[TableAck](response.body()) shouldBe Right(TableAck("table-1"))
    registry.find("table-1").map(_.freeSeats) shouldBe Some(3)

  test("a heartbeat revives a table that had been marked unavailable"):
    register()
    registry.markStatus("table-1", TableStatus.Unavailable)
    heartbeat("table-1", 5)
    registry.find("table-1").map(_.status) shouldBe Some(TableStatus.Available)

  test("a heartbeat from a table the lobby does not know gets 404, so that the table can register again"):
    val response = heartbeat("ghost", 1)
    response.statusCode() shouldBe 404
    reason(response) should include("ghost")

  test("a heartbeat with impossible free seats gets 400 and changes nothing"):
    register()
    heartbeat("table-1", Game.MaxPlayersNum + 1).statusCode() shouldBe 400
    registry.find("table-1").map(_.freeSeats) shouldBe Some(Game.MaxPlayersNum)
