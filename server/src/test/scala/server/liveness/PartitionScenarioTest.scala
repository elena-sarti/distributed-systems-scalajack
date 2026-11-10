package server.liveness

import protocol.ClientMessage.*
import protocol.LobbyApi
import protocol.ServerMessage
import protocol.ServerMessage.{ActionRequest, ErrorMessage}
import server.auth.TestLobby
import server.lobby.{BackupExclusionEndpoint, HeartbeatMonitor, PrimaryStartEndpoint, PromotionArbiter, PromotionEndpoint, TableHeartbeatEndpoint, TableRegistrationEndpoint, TableRegistry}
import server.replication.*
import server.{Cashier, Connection, ExclusionGateway, HttpLobbyGateway, PromotionAnswer, StartAnswer, TableAnnouncer, TableHandler}
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
  private var arbiter: PromotionArbiter       = _
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
    arbiter = PromotionArbiter(registry)
    lobby = TestLobby(Map(
      LobbyApi.PromotionPath      -> PromotionEndpoint(arbiter),
      LobbyApi.ExclusionPath      -> BackupExclusionEndpoint(arbiter),
      LobbyApi.TableRegisterPath  -> TableRegistrationEndpoint(registry, arbiter),
      LobbyApi.TableHeartbeatPath -> TableHeartbeatEndpoint(registry, arbiter),
      LobbyApi.StartPath          -> PrimaryStartEndpoint(arbiter)))
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
    primary.isReplaced shouldBe true // and so it steps down

  test("when the replicas see each other again the backup gets the whole state, and can again take over if the primary fails"):
    aliceSeated()
    link.up = false
    afterTheTimeout()
    period(lobbyHearsPrimary = true)
    primary.isAlone shouldBe true
    link.up = true // the partition heals: the heartbeats of the primary, with its new epoch, reach the backup again
    primarySees.heardFrom(Peer.Backup)
    backupSees.heardFrom(Peer.Primary)
    period(lobbyHearsPrimary = true)
    primary.onTick()
    primary.isAlone shouldBe false
    replica.latest.map(_.epoch) shouldBe Some(11L)
    now = now.plus(timeout).plusSeconds(1) // now the primary crashes: it asks nobody for anything any more
    toLobby.up = false
    period(lobbyHearsPrimary = false)
    period(lobbyHearsPrimary = false)
    takenOver.toList shouldBe List(12L)

  test("a primary cut off from everybody comes back after its backup was promoted: the lobby refuses it and it steps down"):
    val alice = aliceSeated()
    val lobbyUrl  = s"http://localhost:${lobby.port}"
    val announcer = TableAnnouncer(HttpLobbyGateway(lobbyUrl, epoch = () => Some(primary.currentEpoch)), "table-1",
      "primary-host", 9091, primary, () => primary.stepDown())
    announcer.tick() // the primary is registered, with its epoch
    registry.register("table-1", "primary-host", 9091, t0) // as if heard at t0, on the clock of the test
    link.up = false
    toLobby.up = false
    afterTheTimeout()
    period(lobbyHearsPrimary = false)
    period(lobbyHearsPrimary = false)
    takenOver.toList shouldBe List(11L)
    arbiter.admits("table-1", 11) // the promoted node announces itself with its epoch
    registry.register("table-1", "backup-host", 9191, now)
    announcer.tick() // the partition heals: the next heartbeat of the old primary reaches the lobby
    primary.isReplaced shouldBe true
    registry.find("table-1").map(_.host) shouldBe Some("backup-host") // the players are still sent to the promoted node
    primary.onMessage(alice, PlaceBet(10))
    alice.inbox.last shouldBe ErrorMessage(ReplicatedTable.TableMoved)
    // and if the old primary restarts, it is not given the table back while the promoted node plays it
    HttpLobbyGateway(lobbyUrl).requestStart("table-1") shouldBe StartAnswer.Denied(409)

