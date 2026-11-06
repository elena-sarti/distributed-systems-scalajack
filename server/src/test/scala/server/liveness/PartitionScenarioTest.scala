package server.liveness

import protocol.ClientMessage.*
import protocol.LobbyApi
import protocol.ServerMessage
import protocol.ServerMessage.{ActionRequest, ErrorMessage}
import server.auth.TestLobby
import server.lobby.{BackupExclusionEndpoint, HeartbeatMonitor, PromotionArbiter, PromotionEndpoint, TableRegistry}
import server.replication.*
import server.{Cashier, Connection, ExclusionGateway, HttpLobbyGateway, PromotionAnswer, TableHandler}
import utils.ModelExports.{Deck, Game, StandardCard, Suit, Value}
import utils.TestExports.*
import java.time.{Duration, Instant}
import scala.collection.mutable.ListBuffer

/** The rows of the decision record in which the two replicas lose each other, played with the real lobby over HTTP.
 * The clock is moved by the test, and the replication between the two replicas is cut by the test. */
class PartitionScenarioTest extends AnyFunSuite with BeforeAndAfterEach:

  private val t0      = Instant.parse("2026-01-01T10:00:00Z")
  private val timeout = Duration.ofSeconds(6)

  private final class FakeConnection(val id: String) extends Connection:
    val inbox = ListBuffer[ServerMessage]()
    def send(message: ServerMessage): Unit = inbox += message

  /** The link between the primary and its backup, which the test can cut. */
  private final class Link(backup: BackupReplica) extends Replicator:
    var up = true
    def replicate(update: ReplicaUpdate): Boolean = up && backup.store(update)

  /** The way from the primary to the lobby, which the test can cut. */
  private final class LobbyLink(lobby: ExclusionGateway) extends ExclusionGateway:
    var up = true
    def requestExclusion(tableId: String, epoch: Long): PromotionAnswer =
      if up then lobby.requestExclusion(tableId, epoch) else PromotionAnswer.Unreachable

  private var now: Instant                    = _
  private var registry: TableRegistry         = _
  private var monitor: HeartbeatMonitor       = _
  private var lobby: TestLobby                = _
  private var replica: BackupReplica          = _
  private var link: Link                      = _
  private var primary: ReplicatedTable        = _
  private var primarySees: FailureDetector[Peer] = _
  private var backupSees: FailureDetector[Peer]  = _
  private var toLobby: LobbyLink              = _
  private var excluder: BackupExcluder        = _
  private var promoter: BackupPromoter        = _
  private var takenOver: ListBuffer[Long]     = _

  override def beforeEach(): Unit =
    now = t0
    registry = TableRegistry()
    registry.register("table-1", "primary-host", 9091, t0)
    monitor = HeartbeatMonitor(registry, timeout, () => now)
    val arbiter = PromotionArbiter(registry)
    lobby = TestLobby(Map(LobbyApi.PromotionPath -> PromotionEndpoint(arbiter), LobbyApi.ExclusionPath -> BackupExclusionEndpoint(arbiter)))
    val gateway = HttpLobbyGateway(s"http://localhost:${lobby.port}")
    replica = BackupReplica()
    link = Link(replica)
    val deck = List(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Six, Value.Ten).map(StandardCard(Suit.Diamonds, _))
    primary = ReplicatedTable(Cashier.withoutMemory(TableHandler.DefaultBalance), link, epoch = 10,
      newTable = cashier => TableHandler(cashier = cashier, newGame = () => Game(List.empty, Deck.testDeck(deck*)), clock = () => now))
    primarySees = FailureDetector[Peer](timeout, LoggingFailureListener("primary"), () => now)
    backupSees  = FailureDetector[Peer](timeout, LoggingFailureListener("backup"), () => now)
    primarySees.watch(Peer.Backup)
    backupSees.watch(Peer.Primary)
    toLobby = LobbyLink(gateway)
    excluder = BackupExcluder("table-1", primary, primarySees, toLobby)
    takenOver = ListBuffer.empty
    promoter = BackupPromoter("table-1", replica, backupSees, gateway, (_, epoch) => takenOver += epoch)

  override def afterEach(): Unit = lobby.stop()

  /** One period of every node, after the replicas have not heard each other since t0. */
  private def period(lobbyHearsPrimary: Boolean): Unit =
    if lobbyHearsPrimary then registry.heartbeat("table-1", 6, now)
    monitor.check()
    primarySees.check()
    backupSees.check()
    excluder.tick()
    promoter.tick()

  private def afterTheTimeout(): Unit = now = t0.plus(timeout).plusMillis(1)

  private def aliceSeated(): FakeConnection =
    val alice = FakeConnection("alice")
    primary.onMessage(alice, JoinTable("Alice"))
    alice

  test("the replicas are cut off from each other: the primary goes on alone and the backup is not promoted"):
    val alice = aliceSeated()
    link.up = false
    afterTheTimeout()
    period(lobbyHearsPrimary = true)
    primary.isAlone shouldBe true
    takenOver shouldBe empty
    primary.onMessage(alice, PlaceBet(10)) // the table is not paused any more
    alice.inbox.exists(_.isInstanceOf[ActionRequest]) shouldBe true

  test("while the backup is unreachable and the lobby has not agreed yet, the moves are refused"):
    val alice = aliceSeated()
    link.up = false
    primary.onMessage(alice, PlaceBet(10))
    alice.inbox.last shouldBe ErrorMessage(ReplicatedTable.TablePaused)

  test("an excluded backup is not promoted even if the primary fails afterwards: it does not have the last moves"):
    aliceSeated()
    link.up = false
    afterTheTimeout()
    period(lobbyHearsPrimary = true)
    primary.isAlone shouldBe true
    now = now.plus(timeout).plusSeconds(1) // and now the primary crashes too
    period(lobbyHearsPrimary = false)
    period(lobbyHearsPrimary = false)
    takenOver shouldBe empty

  test("the primary reaches neither the backup nor the lobby: it stays paused, the backup is promoted, and the primary can never go on alone"):
    aliceSeated()
    link.up = false
    toLobby.up = false
    afterTheTimeout()
    period(lobbyHearsPrimary = false)
    period(lobbyHearsPrimary = false)
    primary.isAlone shouldBe false
    takenOver.toList shouldBe List(11L)
    toLobby.up = true // the partition heals
    period(lobbyHearsPrimary = true)
    primary.isAlone shouldBe false // its epoch has been used by the promotion
