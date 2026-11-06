package server.replication

import server.liveness.{FailureDetector, LoggingFailureListener, Peer}
import server.{PromotionAnswer, PromotionGateway}
import utils.TestExports.*
import java.time.{Duration, Instant}
import scala.collection.mutable.ListBuffer

class BackupPromoterTest extends AnyFunSuite with BeforeAndAfterEach:

  private val t0      = Instant.parse("2026-01-01T10:00:00Z")
  private val timeout = Duration.ofSeconds(6)
  private val state   = TableSnapshot(7, List.empty, List.empty, List.empty, List.empty, cutCardInDeck = true,
    List.empty, PhaseSnapshot.Idle, None)

  /** A lobby that answers what the test decides, and remembers what it was asked. */
  private final class FakeLobby(var answer: PromotionAnswer) extends PromotionGateway:
    val requests = ListBuffer.empty[(String, Long)]
    def requestPromotion(tableId: String, epoch: Long): PromotionAnswer =
      requests += ((tableId, epoch))
      answer

  private var now: Instant                    = _
  private var peers: FailureDetector[Peer]    = _
  private var replica: BackupReplica          = _
  private var takenOver: ListBuffer[(Option[TableSnapshot], Long)] = _

  override def beforeEach(): Unit =
    now = t0
    peers = FailureDetector[Peer](timeout, LoggingFailureListener("backup"), () => now)
    peers.watch(Peer.Primary)
    replica = BackupReplica()
    takenOver = ListBuffer.empty

  private def promoter(lobby: FakeLobby) =
    BackupPromoter("table-1", replica, peers, lobby, (snapshot, epoch) => takenOver += ((snapshot, epoch)))

  private def primaryGoesSilent(): Unit =
    now = t0.plus(timeout).plusMillis(1)
    peers.check()

  test("while the primary is heard the backup does not ask for anything"):
    val lobby = FakeLobby(PromotionAnswer.Granted)
    promoter(lobby).tick()
    lobby.requests shouldBe empty
    takenOver shouldBe empty

  test("when the primary is silent and the lobby agrees, the backup takes over from the last state with the next epoch"):
    replica.store(ReplicaUpdate(10, 3, state))
    primaryGoesSilent()
    val lobby    = FakeLobby(PromotionAnswer.Granted)
    val promoted = promoter(lobby)
    promoted.tick()
    lobby.requests.toList shouldBe List(("table-1", 11L))
    takenOver.toList shouldBe List((Some(state), 11L))
    promoted.isPromoted shouldBe true

  test("once promoted the backup follows nobody: the old primary is refused"):
    replica.store(ReplicaUpdate(10, 3, state))
    primaryGoesSilent()
    promoter(FakeLobby(PromotionAnswer.Granted)).tick()
    replica.store(ReplicaUpdate(10, 4, state)) shouldBe false
    replica.follows(10) shouldBe false

  test("a primary that never sent a state is replaced by an empty table, with epoch 1"):
    primaryGoesSilent()
    promoter(FakeLobby(PromotionAnswer.Granted)).tick()
    takenOver.toList shouldBe List((None, 1L))

  test("a promotion the lobby refuses changes nothing, and is asked again at the next period"):
    replica.store(ReplicaUpdate(10, 3, state))
    primaryGoesSilent()
    val lobby    = FakeLobby(PromotionAnswer.Denied(409)) // e.g. the lobby still hears the primary
    val promoted = promoter(lobby)
    promoted.tick()
    promoted.tick()
    takenOver shouldBe empty
    promoted.isPromoted shouldBe false
    replica.follows(10) shouldBe true // still a backup
    lobby.requests should have size 2
    lobby.answer = PromotionAnswer.Granted
    promoted.tick()
    takenOver should have size 1

  test("with the lobby unreachable there is no majority: the backup stays a backup and keeps trying"):
    primaryGoesSilent()
    val lobby    = FakeLobby(PromotionAnswer.Unreachable)
    val promoted = promoter(lobby)
    promoted.tick()
    takenOver shouldBe empty
    lobby.answer = PromotionAnswer.Granted
    promoted.tick()
    takenOver should have size 1

  test("a primary that comes back before the promotion is granted cancels the attempts"):
    primaryGoesSilent()
    val lobby    = FakeLobby(PromotionAnswer.Denied(409))
    val promoted = promoter(lobby)
    promoted.tick()
    peers.heardFrom(Peer.Primary)
    promoted.tick()
    lobby.requests should have size 1

  test("the take over happens once, even if the primary stays silent for many periods"):
    primaryGoesSilent()
    val lobby    = FakeLobby(PromotionAnswer.Granted)
    val promoted = promoter(lobby)
    (1 to 5).foreach(_ => promoted.tick())
    takenOver should have size 1
    lobby.requests should have size 1
