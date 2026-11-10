package server.replication

import protocol.LobbyApi
import server.auth.TestLobby
import server.auth.TestLobby.reason
import server.liveness.{FailureDetector, LoggingFailureListener, Peer, PeerStatus}
import utils.TestExports.*
import java.time.{Duration, Instant}

class PrimaryHeartbeatEndpointTest extends AnyFunSuite with BeforeAndAfterEach:

  private val t0    = Instant.parse("2026-01-01T10:00:00Z")
  private val state = TableSnapshot(0, List.empty, List.empty, List.empty, List.empty, cutCardInDeck = true,
    List.empty, PhaseSnapshot.Idle, None)

  private var now: Instant                    = _
  private var backup: BackupReplica           = _
  private var detector: FailureDetector[Peer] = _
  private var node: TestLobby                 = _ // any node with an HTTP server will do

  override def beforeEach(): Unit =
    now = t0
    backup = BackupReplica()
    detector = FailureDetector[Peer](Duration.ofSeconds(6), LoggingFailureListener("backup"), () => now)
    detector.watch(Peer.Primary)
    node = TestLobby(Map(PrimaryHeartbeatEndpoint.Path -> PrimaryHeartbeatEndpoint(backup, detector)))

  override def afterEach(): Unit = node.stop()

  private def heartbeat(epoch: Long) =
    node.post(PrimaryHeartbeatEndpoint.Path, LobbyApi.encode(ReplicaHeartbeat(epoch)))

  test("a heartbeat survives a JSON round-trip"):
    LobbyApi.decode[ReplicaHeartbeat](LobbyApi.encode(ReplicaHeartbeat(7))) shouldBe Right(ReplicaHeartbeat(7))
    LobbyApi.decode[HeartbeatAck](LobbyApi.encode(HeartbeatAck(7))) shouldBe Right(HeartbeatAck(7))

  test("the heartbeat of the primary gets 200 and is a sign of life"):
    now = t0.plusSeconds(5)
    val response = heartbeat(1)
    response.statusCode() shouldBe 200
    LobbyApi.decode[HeartbeatAck](response.body()) shouldBe Right(HeartbeatAck(1))
    now = t0.plusSeconds(10) // more than the timeout after the start, but not after the heartbeat
    detector.check() shouldBe empty

  test("without heartbeats the primary is suspected"):
    now = t0.plusSeconds(7)
    detector.check() shouldBe Seq(Peer.Primary)

  test("the heartbeat of a primary that has been replaced gets 409 and is not a sign of life"):
    backup.store(ReplicaUpdate(2, 1, state))
    val response = heartbeat(1)
    response.statusCode() shouldBe 409
    reason(response) shouldBe ReplicaEndpoint.Superseded
    now = t0.plusSeconds(7)
    detector.check() shouldBe Seq(Peer.Primary)

  test("a suspected primary that sends a heartbeat is alive again"):
    now = t0.plusSeconds(7)
    detector.check()
    heartbeat(1)
    detector.status(Peer.Primary) shouldBe Some(PeerStatus.Alive)

  test("a body that is not a heartbeat gets 400, and only POST is accepted"):
    node.post(PrimaryHeartbeatEndpoint.Path, "not json").statusCode() shouldBe 400
    node.get(PrimaryHeartbeatEndpoint.Path).statusCode() shouldBe 405
