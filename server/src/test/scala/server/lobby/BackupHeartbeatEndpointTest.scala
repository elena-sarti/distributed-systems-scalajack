package server.lobby

import protocol.LobbyApi
import protocol.LobbyApi.{BackupHeartbeat, TableAck}
import server.auth.TestLobby
import server.auth.TestLobby.reason
import server.liveness.{FailureDetector, FailureListener}
import utils.TestExports.*
import java.time.{Duration, Instant}
import scala.collection.mutable.ListBuffer

class BackupHeartbeatEndpointTest extends AnyFunSuite with BeforeAndAfterEach:

  private val t0 = Instant.parse("2026-01-01T10:00:00Z")

  /** Remembers the backups signalled as unreachable. */
  private final class RecordingListener extends FailureListener[String]:
    def onSuspected(peer: String): Unit = suspected += peer
    def onRecovered(peer: String): Unit = ()

  private var now: Instant                      = _
  private var suspected: ListBuffer[String]     = _
  private var backups: FailureDetector[String]  = _
  private var lobby: TestLobby                  = _
  private var registry: TableRegistry           = _

  override def beforeEach(): Unit =
    now = t0
    suspected = ListBuffer.empty
    registry = TableRegistry()
    backups = FailureDetector[String](Duration.ofSeconds(6), RecordingListener(), () => now)
    lobby = TestLobby(Map(LobbyApi.BackupHeartbeatPath -> BackupHeartbeatEndpoint(backups)))

  override def afterEach(): Unit = lobby.stop()

  private def heartbeat(tableId: String) =
    lobby.post(LobbyApi.BackupHeartbeatPath, LobbyApi.encode(BackupHeartbeat(tableId)))

  test("a heartbeat of a backup gets 200 and the lobby starts watching it"):
    val response = heartbeat("table-1")
    response.statusCode() shouldBe 200
    LobbyApi.decode[TableAck](response.body()) shouldBe Right(TableAck("table-1"))
    backups.status("table-1") should not be empty

  test("a backup that keeps sending heartbeats is not signalled, one that goes silent is, within the timeout"):
    (1 to 5).foreach: n =>
      now = t0.plusSeconds(n * 2L)
      heartbeat("table-1")
      backups.check() shouldBe empty
    now = now.plusSeconds(6).plusMillis(1) // the backup crashes: nothing more arrives
    backups.check() shouldBe Seq("table-1")
    suspected.toList shouldBe List("table-1")

  test("each backup is watched on its own"):
    heartbeat("table-1")
    now = t0.plusSeconds(5)
    heartbeat("table-2")
    now = t0.plusSeconds(7)
    backups.check() shouldBe Seq("table-1")

  test("a heartbeat with a blank table id or a body that is not a heartbeat gets 400 and watches nobody"):
    val blank = heartbeat(" ")
    blank.statusCode() shouldBe 400
    reason(blank) should not be empty
    lobby.post(LobbyApi.BackupHeartbeatPath, "not json").statusCode() shouldBe 400
    backups.status(" ") shouldBe None

  test("only POST is accepted"):
    lobby.get(LobbyApi.BackupHeartbeatPath).statusCode() shouldBe 405

  test("a backup never appears in the registry: it can not be assigned to players"):
    heartbeat("table-1")
    registry.tables shouldBe empty
