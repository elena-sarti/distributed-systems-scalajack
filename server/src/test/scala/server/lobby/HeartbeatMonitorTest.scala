package server.lobby

import server.auth.TestLobby.capturedOutput
import utils.TestExports.*
import java.time.{Duration, Instant}

class HeartbeatMonitorTest extends AnyFunSuite with BeforeAndAfterEach:

  private val t0      = Instant.parse("2026-01-01T10:00:00Z")
  private val timeout = Duration.ofSeconds(6)

  var registry: TableRegistry = _
  var now: Instant            = _
  var monitor: HeartbeatMonitor = _

  override def beforeEach(): Unit =
    registry = TableRegistry()
    now = t0
    monitor = HeartbeatMonitor(registry, timeout, () => now) // the clock is a variable: the tests decide what time it is
    registry.register("table-1", "host", 8090, t0)

  test("a table is still available when the timeout has not passed"):
    now = t0.plus(timeout)
    monitor.check() shouldBe empty
    registry.find("table-1").map(_.status) shouldBe Some(TableStatus.Available)

  test("a table with no heartbeat within the timeout is marked unavailable"):
    now = t0.plus(timeout).plusMillis(1)
    monitor.check().map(_.id) shouldBe Seq("table-1")
    registry.find("table-1").map(_.status) shouldBe Some(TableStatus.Unavailable)
    registry.find("table-1").map(_.isJoinable) shouldBe Some(false)

  test("a table that keeps sending heartbeats is never marked unavailable"):
    (1 to 10).foreach: n =>
      now = t0.plusSeconds(n * 4L) // a heartbeat every 4 s, the timeout is 6 s
      registry.heartbeat("table-1", 7, now)
      monitor.check() shouldBe empty

  test("a table that comes back with a heartbeat is available again"):
    now = t0.plusSeconds(60)
    monitor.check()
    registry.heartbeat("table-1", 5, now)
    registry.find("table-1").map(_.status) shouldBe Some(TableStatus.Available)
    monitor.check() shouldBe empty

  test("the table that went silent is written in the log"):
    now = t0.plusSeconds(60)
    capturedOutput(monitor.check()) should include("table-1")
