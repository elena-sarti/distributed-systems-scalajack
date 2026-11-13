package server

import com.sun.net.httpserver.HttpHandler
import protocol.LobbyApi
import server.lobby.*
import utils.ModelExports.Game
import utils.TestExports.*
import java.time.{Duration, Instant}

/** The table side talking to the lobby endpoints and the monitor
 * noticing when a table goes silent. */
class TableLobbyLinkTest extends AnyFunSuite with BeforeAndAfterEach:

  private val t0 = Instant.parse("2026-01-01T10:00:00Z")

  private final class FakeSeats(var freeSeats: Int) extends SeatAvailability

  var registry: TableRegistry = _
  var lobby: HealthServer     = _
  var gateway: HttpLobbyGateway = _

  override def beforeEach(): Unit =
    registry = TableRegistry()
    val routes: Map[String, HttpHandler] = Map(
      LobbyApi.TableRegisterPath  -> TableRegistrationEndpoint(registry),
      LobbyApi.TableHeartbeatPath -> TableHeartbeatEndpoint(registry)
    )
    lobby = HealthServer(NodeRole.Lobby, 0, routes)
    lobby.start()
    gateway = HttpLobbyGateway(s"http://localhost:${lobby.boundPort}")

  override def afterEach(): Unit = lobby.stop()

  test("the gateway registers a table and sends its heartbeats to the lobby"):
    gateway.register("table-1", "localhost", 9091) shouldBe true
    gateway.heartbeat("table-1", 2) shouldBe HeartbeatResult.Acknowledged
    registry.find("table-1").map(_.freeSeats) shouldBe Some(2)

  test("the gateway tells that the lobby does not know a table"):
    gateway.heartbeat("ghost", 2) shouldBe HeartbeatResult.UnknownTable

  test("the gateway reports a registration refused by the lobby as a failure"):
    gateway.register(" ", "localhost", 9091) shouldBe false

  test("when the lobby cannot be reached nothing throws: registration fails and the heartbeat is unreachable"):
    val nobodyThere = HttpLobbyGateway("http://localhost:1") // nothing listens on port 1: connection refused
    nobodyThere.register("table-1", "localhost", 9091) shouldBe false
    nobodyThere.heartbeat("table-1", 2) shouldBe HeartbeatResult.Unreachable

  test("a table that announces itself is available with its real seats; when it falls silent it is marked unavailable, and it is available again when it comes back"):
    val seats     = FakeSeats(Game.MaxPlayersNum)
    val announcer = TableAnnouncer(gateway, "table-1", "localhost", 9091, seats)
    announcer.tick()
    seats.freeSeats = 4
    announcer.tick()
    registry.find("table-1").map(t => (t.freeSeats, t.status)) shouldBe Some((4, TableStatus.Available))

    // the table sends nothing for longer than the timeout
    val timeout = Duration.ofSeconds(6)
    HeartbeatMonitor(registry, timeout, () => Instant.now().plus(timeout).plusSeconds(1)).check().map(_.id) shouldBe Seq("table-1")
    registry.find("table-1").map(_.isJoinable) shouldBe Some(false)

    announcer.tick() // it is back
    registry.find("table-1").map(t => (t.freeSeats, t.status)) shouldBe Some((4, TableStatus.Available))
