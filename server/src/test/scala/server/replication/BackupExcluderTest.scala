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

  override def beforeEach(): Unit =
    now = t0
    peers = FailureDetector[Peer](timeout, LoggingFailureListener("primary"), () => now)
    peers.watch(Peer.Backup)
    table = ReplicatedTable(Cashier.withoutMemory(100), _ => false, epoch = 10) // a backup that never answers

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
