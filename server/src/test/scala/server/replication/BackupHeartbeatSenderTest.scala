package server.replication

import server.liveness.{FailureDetector, LoggingFailureListener, Peer}
import server.{HealthServer, NodeRole}
import utils.TestExports.*
import java.net.ServerSocket
import java.time.{Duration, Instant}
import scala.util.Using

class BackupHeartbeatSenderTest extends AnyFunSuite with BeforeAndAfterEach:

  private val t0      = Instant.parse("2026-01-01T10:00:00Z")
  private val timeout = Duration.ofSeconds(6)

  /** A backup that answers what the test decides, and remembers the epochs it was sent. */
  private final class FakeBackup(var answers: Boolean = true) extends BackupHeartbeats:
    var epochs = List.empty[Long]
    def heartbeat(epoch: Long): Boolean =
      epochs = epochs :+ epoch
      answers

  private var now: Instant                    = _
  private var detector: FailureDetector[Peer] = _

  override def beforeEach(): Unit =
    now = t0
    detector = FailureDetector[Peer](timeout, LoggingFailureListener("primary"), () => now)
    detector.watch(Peer.Backup)

  test("every tick sends a heartbeat carrying the epoch of the primary"):
    val backup = FakeBackup()
    val sender = BackupHeartbeatSender(backup, 42, detector)
    sender.tick()
    sender.tick()
    backup.epochs shouldBe List(42L, 42L)

  test("a backup that answers is never suspected"):
    val sender = BackupHeartbeatSender(FakeBackup(), 1, detector)
    (1 to 10).foreach: n =>
      now = t0.plusSeconds(n * 4L)
      sender.tick()
      detector.check() shouldBe empty

  test("a backup that stops answering is suspected after the timeout, not before"):
    val backup = FakeBackup()
    val sender = BackupHeartbeatSender(backup, 1, detector)
    now = t0.plusSeconds(2)
    sender.tick()
    backup.answers = false
    now = t0.plusSeconds(2).plus(timeout)
    sender.tick()
    detector.check() shouldBe empty // the last answer was exactly one timeout ago
    now = now.plusMillis(1)
    detector.check() shouldBe Seq(Peer.Backup)

  test("over HTTP: the real backup answers the heartbeat, and nobody answers when it is not there"):
    val replica = BackupReplica()
    val node    = HealthServer(NodeRole.Backup, 0, Map(
      PrimaryHeartbeatEndpoint.Path -> PrimaryHeartbeatEndpoint(replica, FailureDetector[Peer](timeout, LoggingFailureListener("backup")))))
    node.start()
    try HttpReplicator(s"http://localhost:${node.boundPort}").heartbeat(1) shouldBe true
    finally node.stop()
    val port = Using.resource(ServerSocket(0))(_.getLocalPort) // a port nobody listens on
    HttpReplicator(s"http://localhost:$port", Duration.ofMillis(500)).heartbeat(1) shouldBe false
