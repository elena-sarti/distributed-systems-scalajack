package server.lobby

import protocol.LobbyApi
import protocol.LobbyApi.{LocateRequest, TableAddress}
import server.auth.{InMemoryUserRepository, SessionTokens, TestLobby}
import server.auth.TestLobby.reason
import utils.TestExports.*

class TableLocationEndpointTest extends AnyFunSuite with BeforeAndAfterEach:

  private var users: InMemoryUserRepository = _
  private var registry: TableRegistry       = _
  private var tokens: SessionTokens         = _
  private var lobby: TestLobby              = _

  override def beforeEach(): Unit =
    users = InMemoryUserRepository()
    users.create("Alice", "hash")
    registry = TableRegistry()
    tokens = SessionTokens("a test secret that is long enough for HMAC".getBytes("UTF-8"))
    lobby = TestLobby(Map(LobbyApi.LocatePath -> TableLocationEndpoint(users, registry, tokens)))

  override def afterEach(): Unit = lobby.stop()

  private def locate(token: String = tokens.issue("Alice")) =
    lobby.post(LobbyApi.LocatePath, LobbyApi.encode(LocateRequest(token)))

  test("a player seated at a table is told where the table is now"):
    registry.register("table-1", "primary-host", 9091)
    users.sitDown("Alice", "table-1")
    val response = locate()
    response.statusCode() shouldBe 200
    LobbyApi.decode[TableAddress](response.body()) shouldBe Right(TableAddress("table-1", "primary-host", 9091))

  test("after the failover the same request gives the address of the node that took over"):
    registry.register("table-1", "primary-host", 9091)
    users.sitDown("Alice", "table-1")
    registry.markStatus("table-1", TableStatus.Unavailable) // the primary is gone
    locate().statusCode() shouldBe 503 // the backup has not announced itself yet: try again
    registry.register("table-1", "backup-host", 9191) // it has taken over: the entry of the primary is replaced
    LobbyApi.decode[TableAddress](locate().body()) shouldBe Right(TableAddress("table-1", "backup-host", 9191))

  test("a player who is not seated anywhere gets 404: the seat has been given up"):
    val response = locate()
    response.statusCode() shouldBe 404
    reason(response) shouldBe TableLocationEndpoint.NotSeated

  test("a table the lobby does not know is not reachable yet: 503, try again"):
    users.sitDown("Alice", "table-9")
    val response = locate()
    response.statusCode() shouldBe 503
    reason(response) shouldBe TableLocationEndpoint.NotAvailable

  test("an invalid token gets 401, a body that is not a request 400, and only POST is accepted"):
    locate("not a token").statusCode() shouldBe 401
    lobby.post(LobbyApi.LocatePath, "not json").statusCode() shouldBe 400
    lobby.get(LobbyApi.LocatePath).statusCode() shouldBe 405

  test("the player is the one in the token: the address of somebody else's table can not be asked"):
    users.create("Bob", "hash")
    registry.register("table-1", "host", 9091)
    users.sitDown("Bob", "table-1")
    locate(tokens.issue("Alice")).statusCode() shouldBe 404 // Alice is not seated, whatever Bob is doing
