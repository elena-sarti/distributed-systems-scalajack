package server.lobby

import utils.ModelExports.Game
import utils.TestExports.*
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
