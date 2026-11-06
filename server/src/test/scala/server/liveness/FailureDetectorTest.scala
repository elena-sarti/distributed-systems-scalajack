package server.liveness

import server.auth.TestLobby.capturedOutput
import utils.TestExports.*
import java.time.{Duration, Instant}
import scala.collection.mutable.ListBuffer

class FailureDetectorTest extends AnyFunSuite with BeforeAndAfterEach:

  private val t0      = Instant.parse("2026-01-01T10:00:00Z")
  private val timeout = Duration.ofSeconds(6)

  /** Records what the detector reports. */
  private final class RecordingListener extends FailureListener[String]:
    val suspected = ListBuffer.empty[String]
    val recovered = ListBuffer.empty[String]
    def onSuspected(peer: String): Unit = suspected += peer
    def onRecovered(peer: String): Unit = recovered += peer

  private var now: Instant                  = _
  private var listener: RecordingListener   = _
  private var detector: FailureDetector[String] = _

  override def beforeEach(): Unit =
    now = t0 // the clock is a variable: the tests decide what time it is
    listener = RecordingListener()
    detector = FailureDetector[String](timeout, listener, () => now)
    detector.watch("backup")

  test("a peer is alive while the timeout has not passed, the boundary included"):
    now = t0.plus(timeout)
    detector.check() shouldBe empty
    detector.status("backup") shouldBe Some(PeerStatus.Alive)

  test("a peer silent for longer than the timeout is suspected and signalled"):
    now = t0.plus(timeout).plusMillis(1)
    detector.check() shouldBe Seq("backup")
    detector.isSuspected("backup") shouldBe true
    listener.suspected.toList shouldBe List("backup")

  test("a suspected peer is signalled once, not at every check"):
    now = t0.plusSeconds(60)
    detector.check()
    detector.check() shouldBe empty
    listener.suspected should have size 1

  test("a peer that keeps sending signs of life is never suspected"):
    (1 to 10).foreach: n =>
      now = t0.plusSeconds(n * 4L) // a heartbeat every 4 s, the timeout is 6 s
      detector.heardFrom("backup")
      detector.check() shouldBe empty

  test("a suspected peer that is heard again is alive, and the recovery is signalled"):
    now = t0.plusSeconds(60)
    detector.check()
    detector.heardFrom("backup")
    detector.isSuspected("backup") shouldBe false
    listener.recovered.toList shouldBe List("backup")
    detector.check() shouldBe empty

  test("a sign of life from an alive peer is not a recovery"):
    detector.heardFrom("backup")
    listener.recovered shouldBe empty

  test("a peer starts with a full timeout from the moment it is watched, not from the start of the detector"):
    now = t0.plusSeconds(100)
    detector.watch("lobby")
    now = now.plus(timeout)
    detector.check() shouldBe Seq("backup") // the lobby was watched later: it still has time
    now = now.plusMillis(1)
    detector.check() shouldBe Seq("lobby")

  test("watching a peer again does not give it more time"):
    now = t0.plusSeconds(5)
    detector.watch("backup")
    now = t0.plus(timeout).plusMillis(1)
    detector.check() shouldBe Seq("backup")

  test("a peer heard without being watched starts being watched"):
    detector.heardFrom("table-9")
    detector.status("table-9") shouldBe Some(PeerStatus.Alive)
    now = t0.plusSeconds(60)
    detector.check().toSet shouldBe Set("backup", "table-9")

  test("the peers are independent: one silent does not suspect the other"):
    detector.watch("lobby")
    now = t0.plusSeconds(5)
    detector.heardFrom("lobby")
    now = t0.plus(timeout).plusMillis(1)
    detector.check() shouldBe Seq("backup")
    detector.isSuspected("lobby") shouldBe false

  test("a peer that is not watched has no status"):
    detector.status("ghost") shouldBe None
    detector.isSuspected("ghost") shouldBe false

  test("the logging listener names who is unreachable and who is back"):
    val logging = FailureDetector[Peer](timeout, LoggingFailureListener("table table-1"), () => now)
    logging.watch(Peer.Backup)
    now = t0.plusSeconds(60)
    capturedOutput(logging.check()) should (include("table table-1") and include("Backup") and include("unreachable"))
    capturedOutput(logging.heardFrom(Peer.Backup)) should include("reachable again")

  test("signs of life and checks from different threads never lose a peer"):
    val threads = (1 to 8).map: n =>
      Thread(() => (1 to 200).foreach(i => { detector.heardFrom(s"peer-$n"); detector.check() }))
    threads.foreach(_.start())
    threads.foreach(_.join())
    (1 to 8).foreach(n => detector.status(s"peer-$n") shouldBe Some(PeerStatus.Alive))
