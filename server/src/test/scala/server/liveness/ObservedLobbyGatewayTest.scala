package server.liveness

import server.{HeartbeatResult, LobbyGateway}
import utils.TestExports.*
import java.time.{Duration, Instant}

class ObservedLobbyGatewayTest extends AnyFunSuite with BeforeAndAfterEach:

  private val t0      = Instant.parse("2026-01-01T10:00:00Z")
  private val timeout = Duration.ofSeconds(6)

  /** A lobby that answers what the test decides. */
  private final class FakeGateway extends LobbyGateway:
    var registrationWorks = true
    var answer            = HeartbeatResult.Acknowledged
    def register(id: String, host: String, wsPort: Int): Boolean = registrationWorks
    def heartbeat(id: String, freeSeats: Int): HeartbeatResult   = answer
    def backupHeartbeat(tableId: String): HeartbeatResult        = answer

  private var now: Instant               = _
  private var detector: FailureDetector[Peer] = _
  private var inner: FakeGateway         = _
  private var gateway: ObservedLobbyGateway = _

  override def beforeEach(): Unit =
    now = t0
    detector = FailureDetector[Peer](timeout, LoggingFailureListener("test"), () => now)
    detector.watch(Peer.Lobby)
    inner = FakeGateway()
    gateway = ObservedLobbyGateway(inner, detector)

  /** After more than the timeout, is the lobby suspected? */
  private def lobbySuspectedLater: Boolean =
    now = t0.plus(timeout).plusMillis(1)
    detector.check().contains(Peer.Lobby)

  test("the answers of the lobby are passed on unchanged"):
    gateway.heartbeat("table-1", 3) shouldBe HeartbeatResult.Acknowledged
    inner.answer = HeartbeatResult.UnknownTable
    gateway.backupHeartbeat("table-1") shouldBe HeartbeatResult.UnknownTable

  test("a heartbeat that the lobby acknowledges is a sign of life"):
    now = t0.plusSeconds(5)
    gateway.heartbeat("table-1", 3)
    lobbySuspectedLater shouldBe false

  test("a heartbeat of a backup that the lobby acknowledges is a sign of life"):
    now = t0.plusSeconds(5)
    gateway.backupHeartbeat("table-1")
    lobbySuspectedLater shouldBe false

  test("'unknown table' is still an answer: the lobby is alive"):
    inner.answer = HeartbeatResult.UnknownTable
    now = t0.plusSeconds(5)
    gateway.heartbeat("table-1", 3)
    lobbySuspectedLater shouldBe false

  test("a lobby that does not answer is suspected after the timeout"):
    inner.answer = HeartbeatResult.Unreachable
    now = t0.plusSeconds(3)
    gateway.heartbeat("table-1", 3)
    gateway.backupHeartbeat("table-1")
    lobbySuspectedLater shouldBe true

  test("an accepted registration is a sign of life, a refused one proves nothing"):
    now = t0.plusSeconds(5)
    inner.registrationWorks = false
    gateway.register("table-1", "localhost", 9091) shouldBe false
    lobbySuspectedLater shouldBe true

    now = t0.plusSeconds(10)
    inner.registrationWorks = true
    gateway.register("table-1", "localhost", 9091) shouldBe true
    detector.isSuspected(Peer.Lobby) shouldBe false
