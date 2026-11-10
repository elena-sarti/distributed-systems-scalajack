package server.lobby

import utils.ModelExports.Game
import utils.TestExports.*
import java.time.{Duration, Instant}
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

class TableRegistryTest extends AnyFunSuite with BeforeAndAfterEach:

  private val Capacity = Game.MaxPlayersNum

  var registry: TableRegistry = _

  override def beforeEach(): Unit = registry = TableRegistry()

  /** Registers a table that is expected to be valid. */
  private def register(id: String, port: Int = 8090): TableInfo =
    registry.register(id, s"host-$id", port).value

  // Small helper: unwraps the Right of an Either, failing the test with a clear message otherwise.
  extension [A](result: Either[RegistryError, A])
    private def value: A = result.fold(error => fail(s"Unexpected error: $error"), identity)

  test("a new registry knows no table"):
    registry.tables shouldBe empty
    registry.find("table-1") shouldBe None

  test("a registered table is available, with every seat free, and can be found by its id"):
    val info = register("table-1", 9091)
    info shouldBe TableInfo("table-1", "host-table-1", 9091, Capacity, Capacity, TableStatus.Available)
    registry.find("table-1") shouldBe Some(info)

  test("the capacity of the tables is configurable"):
    val small = TableRegistry(capacity = 3)
    small.register("t", "h", 1).value.freeSeats shouldBe 3

  test("a table needs a non blank id and host, and a valid port"):
    registry.register(" ", "host", 8090) shouldBe a[Left[?, ?]]
    registry.register("t", "", 8090) shouldBe a[Left[?, ?]]
    Seq(0, -1, 65536).foreach: port =>
      registry.register("t", "host", port) shouldBe a[Left[?, ?]]
    registry.tables shouldBe empty // nothing registered by the invalid requests

  test("registering again a table (e.g. a restarted node) replaces it with a fresh one"):
    register("table-1")
    registry.updateFreeSeats("table-1", 2).value
    registry.markStatus("table-1", TableStatus.Unavailable).value
    val again = registry.register("table-1", "new-host", 9000).value
    again shouldBe TableInfo("table-1", "new-host", 9000, Capacity, Capacity, TableStatus.Available)
    registry.tables should have size 1

  test("the free seats of a table follow what the table reports"):
    register("table-1")
    registry.updateFreeSeats("table-1", 4).value.freeSeats shouldBe 4
    registry.find("table-1").map(_.freeSeats) shouldBe Some(4)
    registry.updateFreeSeats("table-1", Capacity).value.freeSeats shouldBe Capacity // a seat released is visible at once

  test("the free seats must be between 0 and the capacity"):
    register("table-1")
    Seq(-1, Capacity + 1).foreach: free =>
      registry.updateFreeSeats("table-1", free) shouldBe Left(RegistryError.InvalidFreeSeats(free, Capacity))
    registry.find("table-1").map(_.freeSeats) shouldBe Some(Capacity) // unchanged

  test("the status of a table can be changed, and changed back"):
    register("table-1")
    registry.markStatus("table-1", TableStatus.Unavailable).value.status shouldBe TableStatus.Unavailable
    registry.markStatus("table-1", TableStatus.Available).value.status shouldBe TableStatus.Available

  test("changing the seats or the status of an unknown table is an error"):
    registry.updateFreeSeats("ghost", 1) shouldBe Left(RegistryError.UnknownTable("ghost"))
    registry.markStatus("ghost", TableStatus.Unavailable) shouldBe Left(RegistryError.UnknownTable("ghost"))

  test("changing the status keeps the seats and the other way round"):
    register("table-1")
    registry.updateFreeSeats("table-1", 2).value
    registry.markStatus("table-1", TableStatus.Unavailable).value.freeSeats shouldBe 2
    registry.updateFreeSeats("table-1", 5).value.status shouldBe TableStatus.Unavailable

  test("only an available table with a free seat can be joined"):
    val fresh = register("table-1")
    fresh.isJoinable shouldBe true
    registry.updateFreeSeats("table-1", 0).value.isJoinable shouldBe false // full
    registry.updateFreeSeats("table-1", 1).value
    registry.markStatus("table-1", TableStatus.Unavailable).value.isJoinable shouldBe false

  test("a removed table is no longer known; removing an unknown one changes nothing"):
    register("table-1")
    registry.remove("table-1") shouldBe true
    registry.find("table-1") shouldBe None
    registry.remove("table-1") shouldBe false

  test("the tables are listed in order of id, whatever the order of registration"):
    Seq("table-3", "table-1", "table-2").foreach(register(_))
    registry.tables.map(_.id) shouldBe Seq("table-1", "table-2", "table-3")

  test("the tables registered at the same time from many threads are all in the registry"):
    val threads = 50
    val pool    = Executors.newFixedThreadPool(threads)
    val start   = CountDownLatch(1)
    (1 to threads).foreach: n =>
      pool.execute: () =>
        start.await()
        registry.register(s"table-$n", "host", 8000 + n)
    start.countDown()
    pool.shutdown()
    pool.awaitTermination(10, TimeUnit.SECONDS) shouldBe true
    registry.tables should have size threads

  test("concurrent updates of the same table leave it in one of the written states, never a mix"):
    register("table-1")
    val pool  = Executors.newFixedThreadPool(8)
    val start = CountDownLatch(1)
    (0 to Capacity).foreach: free =>
      pool.execute: () =>
        start.await()
        registry.updateFreeSeats("table-1", free)
    start.countDown()
    pool.shutdown()
    pool.awaitTermination(10, TimeUnit.SECONDS) shouldBe true
    registry.find("table-1").map(_.freeSeats).get should (be >= 0 and be <= Capacity)

  // ---- heartbeats (E2): the registry remembers when each table was last heard from ----

  private val t0 = Instant.parse("2026-01-01T10:00:00Z")

  test("a heartbeat updates the free seats and makes an unavailable table available again"):
    register("table-1")
    registry.markStatus("table-1", TableStatus.Unavailable).value
    val info = registry.heartbeat("table-1", 3, t0).value
    info.freeSeats shouldBe 3
    info.status shouldBe TableStatus.Available

  test("a heartbeat from an unknown table, or with the seats out of range, is refused and changes nothing"):
    registry.heartbeat("ghost", 1, t0) shouldBe Left(RegistryError.UnknownTable("ghost"))
    register("table-1")
    registry.heartbeat("table-1", Capacity + 1, t0) shouldBe Left(RegistryError.InvalidFreeSeats(Capacity + 1, Capacity))
    registry.find("table-1").map(_.freeSeats) shouldBe Some(Capacity)

  test("a table not heard from since the given instant is marked unavailable, the others are left alone"):
    registry.register("silent", "h", 1, t0).value
    registry.register("alive", "h", 2, t0).value
    registry.heartbeat("alive", Capacity, t0.plusSeconds(10)).value
    registry.markSilentUnavailable(t0.plusSeconds(5)).map(_.id) shouldBe Seq("silent")
    registry.find("silent").map(_.status) shouldBe Some(TableStatus.Unavailable)
    registry.find("alive").map(_.status) shouldBe Some(TableStatus.Available)

  test("a table heard exactly at the limit instant is still available, one millisecond older is not"):
    registry.register("table-1", "h", 1, t0).value
    registry.markSilentUnavailable(t0) shouldBe empty
    registry.markSilentUnavailable(t0.plusMillis(1)).map(_.id) shouldBe Seq("table-1")

  test("a table marked unavailable is reported once, and keeps its seats"):
    registry.register("table-1", "h", 1, t0).value
    registry.heartbeat("table-1", 2, t0).value
    registry.markSilentUnavailable(t0.plusSeconds(1)).map(_.freeSeats) shouldBe Seq(2)
    registry.markSilentUnavailable(t0.plusSeconds(2)) shouldBe empty

  test("registering a table counts as hearing from it"):
    registry.register("table-1", "h", 1, t0.plusSeconds(100)).value
    registry.markSilentUnavailable(t0.plusSeconds(50)) shouldBe empty

  // ---- seat reservations (E3): a seat given to a player stays taken until the table reports it ----

  private val hold = Duration.ofSeconds(10)

  private def reserve(from: TableRegistry = registry, at: Instant = t0): Option[String] =
    from.reserveSeat(at, hold).map(_.id)

  test("with no table, no seat can be reserved"):
    reserve() shouldBe None

  test("a reservation is for the first table, in order of id, that has a free seat"):
    Seq("table-2", "table-1").foreach(register(_))
    reserve() shouldBe Some("table-1")

  test("a table gets reservations until all its seats are taken, then the next table does"):
    val small = TableRegistry(capacity = 2)
    Seq("a", "b").foreach(small.register(_, "h", 1))
    Seq.fill(5)(reserve(small)) shouldBe Seq(Some("a"), Some("a"), Some("b"), Some("b"), None)

  test("tables that are unavailable or full are skipped"):
    Seq("table-1", "table-2", "table-3").foreach(register(_))
    registry.markStatus("table-1", TableStatus.Unavailable).value
    registry.updateFreeSeats("table-2", 0).value
    reserve() shouldBe Some("table-3")

  test("a reservation lowers the seats that can be assigned, not the seats the table reports"):
    register("table-1")
    reserve()
    registry.assignableSeats("table-1", t0) shouldBe Some(Capacity - 1)
    registry.find("table-1").map(_.freeSeats) shouldBe Some(Capacity)

  test("the assignable seats of an unknown table are not known"):
    registry.assignableSeats("ghost", t0) shouldBe None

  test("a heartbeat that does not show the player yet does not give the reserved seat back"):
    val small = TableRegistry(capacity = 1)
    small.register("a", "h", 1, t0).value
    reserve(small) shouldBe Some("a")
    small.heartbeat("a", 1, t0.plusSeconds(2)).value // the player has not sat down yet: the table still says 1 free
    reserve(small, t0.plusSeconds(2)) shouldBe None

  test("the assignable seats are never negative, even if the table reports fewer seats than are reserved"):
    register("table-1")
    reserve()
    registry.heartbeat("table-1", 0, t0).value
    registry.assignableSeats("table-1", t0) shouldBe Some(0)

  test("a reservation that is not used expires after the hold time, and the seat can be given again"):
    val small = TableRegistry(capacity = 1)
    small.register("a", "h", 1, t0).value
    reserve(small) shouldBe Some("a")
    reserve(small, t0.plus(hold).minusMillis(1)) shouldBe None // still held
    reserve(small, t0.plus(hold)) shouldBe Some("a")

  test("a table that registers again, like a restarted node, has no reservations"):
    val small = TableRegistry(capacity = 1)
    small.register("a", "h", 1, t0).value
    reserve(small) shouldBe Some("a")
    small.register("a", "h", 1, t0).value
    reserve(small) shouldBe Some("a")

  test("no seat is given twice when many players are assigned at the same time"):
    register("table-1") // 7 seats
    val threads  = 100
    val pool     = Executors.newFixedThreadPool(threads)
    val start    = CountDownLatch(1)
    val assigned = ConcurrentLinkedQueue[String]()
    (1 to threads).foreach: _ =>
      pool.execute: () =>
        start.await()
        reserve().foreach(assigned.add)
    start.countDown()
    pool.shutdown()
    pool.awaitTermination(10, TimeUnit.SECONDS) shouldBe true
    assigned.size shouldBe Capacity
