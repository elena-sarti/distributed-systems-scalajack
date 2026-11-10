package server.liveness

import protocol.LobbyApi
import server.auth.TestLobby
import server.lobby.BackupHeartbeatEndpoint
import server.replication.{BackupHeartbeatSender, BackupReplica, HttpReplicator, PrimaryHeartbeatEndpoint}
import server.{HealthServer, HeartbeatResult, HttpLobbyGateway, NodeRole}
import utils.TestExports.*
import java.time.{Duration, Instant}

/** F3 from end to end: real HTTP between the nodes, a clock that the test moves. For each pair of nodes, a node
 * that stops is signalled by the other within the timeout, and not before. */
class FailureDetectionScenarioTest extends AnyFunSuite with BeforeAndAfterEach:

  private val t0      = Instant.parse("2026-01-01T10:00:00Z")
  private val period  = Duration.ofSeconds(2)
  private val timeout = Duration.ofSeconds(6)

  private var now: Instant = _
  private def detector[P](name: String) = FailureDetector[P](timeout, LoggingFailureListener[P](name), () => now)

  override def beforeEach(): Unit = now = t0

  test("the primary notices that the backup crashed, and the backup that the primary did"):
    val primarySees = detector[Peer]("primary")
    val backupSees  = detector[Peer]("backup")
    primarySees.watch(Peer.Backup)
    backupSees.watch(Peer.Primary)
    val backupNode = HealthServer(NodeRole.Backup, 0, Map(
      PrimaryHeartbeatEndpoint.Path -> PrimaryHeartbeatEndpoint(BackupReplica(), backupSees)))
    backupNode.start()
    val sender = BackupHeartbeatSender(HttpReplicator(s"http://localhost:${backupNode.boundPort}", Duration.ofMillis(500)), 1, primarySees)

    // both nodes are up: heartbeats every period, nobody is suspected
    (1 to 5).foreach: n =>
      now = t0.plusSeconds(n * period.getSeconds)
      sender.tick()
      primarySees.check() shouldBe empty
      backupSees.check() shouldBe empty
    val lastBeat = now

    // the primary crashes: it sends nothing more, the backup signals it after the timeout
    now = lastBeat.plus(timeout)
    backupSees.check() shouldBe empty
    now = now.plusMillis(1)
    backupSees.check() shouldBe Seq(Peer.Primary)

    // the backup crashes too (before the primary comes back): the primary signals it after the timeout
    backupNode.stop()
    sender.tick() // nobody answers
    now = lastBeat.plus(timeout).plusMillis(2)
    primarySees.check() shouldBe Seq(Peer.Backup)

  test("the lobby notices that a backup crashed"):
    val backups = detector[String]("lobby")
    val lobby   = TestLobby(Map(LobbyApi.BackupHeartbeatPath -> BackupHeartbeatEndpoint(backups)))
    try
      val gateway = HttpLobbyGateway(s"http://localhost:${lobby.port}")
      gateway.backupHeartbeat("table-1") shouldBe HeartbeatResult.Acknowledged
      now = t0.plus(timeout)
      backups.check() shouldBe empty
      now = now.plusMillis(1)
      backups.check() shouldBe Seq("table-1")
    finally lobby.stop()

  test("the table and the backup notice that the lobby is unreachable"):
    val tableSees = detector[Peer]("table")
    tableSees.watch(Peer.Lobby)
    val gateway = ObservedLobbyGateway(HttpLobbyGateway("http://localhost:1", Duration.ofMillis(500)), tableSees) // nothing there
    now = t0.plusSeconds(2)
    gateway.heartbeat("table-1", 7) shouldBe HeartbeatResult.Unreachable
    gateway.backupHeartbeat("table-1") shouldBe HeartbeatResult.Unreachable
    tableSees.check() shouldBe empty
    now = t0.plus(timeout).plusMillis(1)
    tableSees.check() shouldBe Seq(Peer.Lobby)
