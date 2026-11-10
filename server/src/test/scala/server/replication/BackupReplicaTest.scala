package server.replication

import utils.TestExports.*

class BackupReplicaTest extends AnyFunSuite:

  private val state = TableSnapshot(0, List.empty, List.empty, List.empty, List.empty, cutCardInDeck = true,
    List.empty, PhaseSnapshot.Idle, None)

  private def update(epoch: Long, sequence: Long, version: Long = 0) =
    ReplicaUpdate(epoch, sequence, state.copy(version = version))

  test("the backup keeps the last update of the primary"):
    val backup = BackupReplica()
    backup.latest shouldBe None
    backup.store(update(1, 1)) shouldBe true
    backup.store(update(1, 2, version = 5)) shouldBe true
    backup.latest shouldBe Some(update(1, 2, version = 5))

  test("an update that arrives after a newer one is acknowledged but does not replace it"):
    val backup = BackupReplica()
    backup.store(update(1, 2, version = 5))
    backup.store(update(1, 1)) shouldBe true
    backup.latest shouldBe Some(update(1, 2, version = 5))

  test("the updates of a replaced primary are refused"):
    val backup = BackupReplica()
    backup.store(update(2, 1))
    backup.store(update(1, 7, version = 9)) shouldBe false
    backup.latest shouldBe Some(update(2, 1))

  test("before any update every primary is followed, afterwards only the current or a newer one"):
    val backup = BackupReplica()
    backup.follows(1) shouldBe true
    backup.store(update(2, 1))
    backup.follows(2) shouldBe true
    backup.follows(3) shouldBe true
    backup.follows(1) shouldBe false

  test("a backup that has become the primary follows nobody: every update and heartbeat is refused"):
    val backup = BackupReplica()
    backup.store(update(1, 1))
    backup.close()
    backup.store(update(1, 2, version = 5)) shouldBe false
    backup.store(update(9, 1)) shouldBe false // not even a primary that restarted with a newer epoch
    backup.follows(9) shouldBe false
    backup.latest shouldBe Some(update(1, 1))

  test("a newer primary is followed from its first update"):
    val backup = BackupReplica()
    backup.store(update(1, 7))
    backup.store(update(2, 1, version = 3)) shouldBe true
    backup.latest shouldBe Some(update(2, 1, version = 3))
