package server.replication

import server.liveness.{FailureDetector, LoggingFailureListener, Peer}
import server.{Cashier, ExclusionGateway, PromotionAnswer}
import utils.TestExports.*
import java.time.{Duration, Instant}
import scala.collection.mutable.ListBuffer

class BackupExcluderTest extends AnyFunSuite with BeforeAndAfterEach:

  private val t0      = Instant.parse("2026-01-01T10:00:00Z")
  private val timeout = Duration.ofSeconds(6)

  /** A lobby that answers what the test decides, and remembers what it was asked. */
  private final class FakeLobby(var answer: PromotionAnswer) extends ExclusionGateway:
    val requests = ListBuffer.empty[(String, Long)]
    def requestExclusion(tableId: String, epoch: Long): PromotionAnswer =
      requests += ((tableId, epoch))
      answer

  private var now: Instant                 = _
  private var peers: FailureDetector[Peer] = _
  private var table: ReplicatedTable       = _
  private var backupStores                = false // whether the backup stores the updates of the table

  override def beforeEach(): Unit =
    now = t0
    peers = FailureDetector[Peer](timeout, LoggingFailureListener("primary"), () => now)
    peers.watch(Peer.Backup)
    backupStores = false
    table = ReplicatedTable(Cashier.withoutMemory(100), _ => backupStores, epoch = 10)

  private def excluder(lobby: FakeLobby) = BackupExcluder("table-1", table, peers, lobby)

  private def backupGoesSilent(): Unit =
    now = t0.plus(timeout).plusMillis(1)
    peers.check()

  test("while the backup is heard the primary does not ask for anything"):
    val lobby = FakeLobby(PromotionAnswer.Granted)
    excluder(lobby).tick()
    lobby.requests shouldBe empty
    table.isAlone shouldBe false

  test("when the backup is silent and the lobby agrees, the table goes on alone with the next epoch"):
    backupGoesSilent()
    val lobby = FakeLobby(PromotionAnswer.Granted)
    excluder(lobby).tick()
    lobby.requests.toList shouldBe List(("table-1", 11L))
    table.isAlone shouldBe true
    table.currentEpoch shouldBe 11

  test("while the lobby does not agree, or cannot be reached, the table stays paused and the request is repeated"):
    backupGoesSilent()
    val lobby    = FakeLobby(PromotionAnswer.Unreachable)
    val excluder = BackupExcluder("table-1", table, peers, lobby)
    excluder.tick()
    lobby.answer = PromotionAnswer.Denied(409) // e.g. the backup has been promoted in the meantime
    excluder.tick()
    table.isAlone shouldBe false
    table.currentEpoch shouldBe 10
    lobby.answer = PromotionAnswer.Granted
    excluder.tick()
    table.isAlone shouldBe true
    lobby.requests should have size 3

  test("once alone the primary asks nothing more"):
    backupGoesSilent()
    val lobby    = FakeLobby(PromotionAnswer.Granted)
    val excluder = BackupExcluder("table-1", table, peers, lobby)
    excluder.tick()
    excluder.tick()
    lobby.requests should have size 1

  test("when the backup answers again the table takes it back, and the lobby is asked nothing"):
    backupGoesSilent()
    val lobby    = FakeLobby(PromotionAnswer.Granted)
    val excluder = BackupExcluder("table-1", table, peers, lobby)
    excluder.tick()
    peers.heardFrom(Peer.Backup) // its heartbeat, with the new epoch
    backupStores = true
    excluder.tick()
    table.onTick() // the state goes to the backup at the next event of the table
    table.isAlone shouldBe false
    lobby.requests should have size 1

  test("a backup taken back that goes silent again is excluded again, with a newer epoch"):
    backupGoesSilent()
    val lobby    = FakeLobby(PromotionAnswer.Granted)
    val excluder = BackupExcluder("table-1", table, peers, lobby)
    excluder.tick()
    peers.heardFrom(Peer.Backup)
    backupStores = true
    excluder.tick()
    table.onTick()
    now = now.plus(timeout).plusSeconds(1)
    peers.check()
    excluder.tick()
    lobby.requests.toList.map(_._2) shouldBe List(11L, 12L)
    table.currentEpoch shouldBe 12

