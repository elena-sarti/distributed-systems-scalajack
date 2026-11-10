package server.replication

import utils.TestExports.*
import server.{HealthServer, NodeRole}
import java.net.ServerSocket
import java.time.Duration
import scala.util.Using

/** The primary and the backup talking over HTTP, as two nodes do. */
class HttpReplicationTest extends AnyFunSuite:

  private val state = TableSnapshot(3, List.empty, List("Alice" -> 10), List.empty, List.empty, cutCardInDeck = true,
    List(SeatSnapshot("Alice", connected = true)), PhaseSnapshot.Betting, Some(1_000L))

  private def withBackup(check: (BackupReplica, HttpReplicator) => Unit): Unit =
    val backup = BackupReplica()
    val node   = HealthServer(NodeRole.Table, 0, Map(ReplicaEndpoint.Path -> ReplicaEndpoint(backup)))
    node.start()
    try check(backup, HttpReplicator(s"http://localhost:${node.boundPort}"))
    finally node.stop()

  test("the backup stores the update sent by the primary and acknowledges it"):
    withBackup: (backup, replicator) =>
      val update = ReplicaUpdate(1, 1, state)
      replicator.replicate(update) shouldBe true
      backup.latest shouldBe Some(update)

  test("a whole table, deck included, fits in an update"):
    val table = server.TableHandler()
    withBackup: (backup, replicator) =>
      val update = ReplicaUpdate(1, 1, table.snapshot)
      replicator.replicate(update) shouldBe true
      backup.latest shouldBe Some(update)

  test("the primary is not acknowledged by a backup that follows a newer primary"):
    withBackup: (backup, replicator) =>
      backup.store(ReplicaUpdate(2, 1, state))
      replicator.replicate(ReplicaUpdate(1, 2, state)) shouldBe false

  test("a backup that cannot be reached does not acknowledge anything"):
    val port = Using.resource(ServerSocket(0))(_.getLocalPort) // a port nobody listens on any more
    val replicator = HttpReplicator(s"http://localhost:$port", Duration.ofMillis(500))
    replicator.replicate(ReplicaUpdate(1, 1, state)) shouldBe false
