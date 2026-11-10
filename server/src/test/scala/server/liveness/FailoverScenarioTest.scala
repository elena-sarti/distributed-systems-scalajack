package server.liveness

import protocol.LobbyApi
import server.auth.TestLobby
import server.lobby.{HeartbeatMonitor, PromotionArbiter, PromotionEndpoint, TableRegistry}
import server.replication.*
import server.{HealthServer, HttpLobbyGateway, NodeRole}
import utils.TestExports.*
import java.time.{Duration, Instant}
import scala.collection.mutable.ListBuffer

/** F4 from end to end: the primary crashes, the backup (over real HTTP, with the real lobby endpoint) is promoted, and
 * the players are sent to its node. The clock is moved by the test. */
class FailoverScenarioTest extends AnyFunSuite with BeforeAndAfterEach:

  private val t0      = Instant.parse("2026-01-01T10:00:00Z")
  private val timeout = Duration.ofSeconds(6)
  private val state   = TableSnapshot(4, List.empty, List.empty, List.empty, List.empty, cutCardInDeck = true,
    List.empty, PhaseSnapshot.Idle, None)

  private var now: Instant                  = _
  private var registry: TableRegistry       = _
  private var monitor: HeartbeatMonitor     = _
  private var lobby: TestLobby              = _
  private var replica: BackupReplica        = _
  private var backupSees: FailureDetector[Peer] = _
  private var takenOver: ListBuffer[(Option[TableSnapshot], Long)] = _
  private var promoter: BackupPromoter      = _

  override def beforeEach(): Unit =
    now = t0
    registry = TableRegistry()
    registry.register("table-1", "primary-host", 9091, t0)
    monitor = HeartbeatMonitor(registry, timeout, () => now)
    lobby = TestLobby(Map(LobbyApi.PromotionPath -> PromotionEndpoint(PromotionArbiter(registry))))
    replica = BackupReplica()
    replica.store(ReplicaUpdate(10, 3, state))
    backupSees = FailureDetector[Peer](timeout, LoggingFailureListener("backup"), () => now)
    backupSees.watch(Peer.Primary)
    takenOver = ListBuffer.empty
    promoter = BackupPromoter("table-1", replica, backupSees, HttpLobbyGateway(s"http://localhost:${lobby.port}"),
      (snapshot, epoch) => takenOver += ((snapshot, epoch)))

  override def afterEach(): Unit = lobby.stop()

  /** One period of the nodes: the lobby checks the tables, the backup checks its primary and tries to be promoted. */
  private def period(): Unit =
    monitor.check()
    backupSees.check()
    promoter.tick()

  test("the primary crashes: the backup is promoted and the lobby sends the players to the promoted node"):
    // the primary was alive until t0 and the crash is noticed by the lobby and by the backup after the timeout
    now = t0.plus(timeout)
    period()
    takenOver shouldBe empty // exactly one timeout of silence is not enough

    now = now.plusMillis(1)
    period()
    takenOver.toList shouldBe List((Some(state), 11L))

    // the promoted node announces itself to the lobby as the table: the entry of the primary is replaced
    registry.register("table-1", "backup-host", 9191, now)
    registry.reserveSeat(now, Duration.ofSeconds(10)).map(t => (t.id, t.host, t.wsPort)) shouldBe Some(("table-1", "backup-host", 9191))

  test("the primary is cut off from the backup only: the lobby still hears it, so the table is not taken over"):
    now = t0.plusSeconds(3)
    registry.heartbeat("table-1", 7, now) // the lobby keeps hearing the primary
    now = t0.plus(timeout).plusSeconds(2)
    registry.heartbeat("table-1", 7, now)
    period()
    takenOver shouldBe empty // the backup suspects the primary, but the lobby does not agree

  test("the lobby notices the silence a period after the backup: the first request is refused, the next is granted"):
    now = t0.plus(timeout).plusMillis(1)
    backupSees.check() // the backup suspects the primary first
    promoter.tick()
    takenOver shouldBe empty // the lobby has not marked the table unavailable yet
    monitor.check()
    promoter.tick()
    takenOver should have size 1

  test("the old primary can not get anything accepted after the promotion"):
    now = t0.plus(timeout).plusMillis(1)
    period()
    takenOver should have size 1
    val node = HealthServer(NodeRole.Backup, 0, Map(
      ReplicaEndpoint.Path          -> ReplicaEndpoint(replica),
      PrimaryHeartbeatEndpoint.Path -> PrimaryHeartbeatEndpoint(replica, backupSees)))
    node.start()
    try
      val old = HttpReplicator(s"http://localhost:${node.boundPort}", Duration.ofMillis(500))
      old.replicate(ReplicaUpdate(10, 4, state)) shouldBe false
      old.heartbeat(10) shouldBe false
    finally node.stop()
