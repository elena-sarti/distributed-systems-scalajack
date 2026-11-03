package server.lobby

import utils.TestExports.*
import java.time.{Duration, Instant}
import scala.collection.mutable.ListBuffer

class TableDispatcherTest extends AnyFunSuite with BeforeAndAfterEach:

  private val t0           = Instant.parse("2026-01-01T10:00:00Z")
  private val seatHold     = Duration.ofSeconds(10)
  private val startTimeout = Duration.ofSeconds(30)

  /** Records which nodes it was asked to start, and starts them (or not) as the test decides. */
  private final class FakeProvisioner extends TableProvisioner:
    var works   = true
    val started = ListBuffer.empty[String]

    def start(tableId: String): Boolean =
      started += tableId
      works

  // tables of 2 seats: easy to fill
  private var registry: TableRegistry       = _
  private var provisioner: FakeProvisioner  = _
  private var now: Instant                  = _
  private var dispatcher: TableDispatcher   = _

  override def beforeEach(): Unit =
    registry = TableRegistry(capacity = 2)
    provisioner = FakeProvisioner()
    now = t0
    dispatcher = dispatcherFor(Seq("table-1", "table-2", "table-3"))

  private def dispatcherFor(pool: Seq[String]): TableDispatcher =
    TableDispatcher(registry, provisioner, pool, seatHold, startTimeout, () => now) // the clock is a variable the tests move

  private def registerTable(id: String): Unit = registry.register(id, "localhost", 9000 + id.last.asDigit, t0)

  private def assignedTo(assignment: Assignment): Option[String] = assignment match
    case Assignment.Assigned(table) => Some(table.id)
    case _                          => None

  /** Takes every seat of the tables there are, so that the next player needs a new table. The players sit
   * down, so each table reports that it is full (the promises of the seats are then no longer what keeps it full). */
  private def fillAll(): Unit =
    registry.tables.foreach: table =>
      (1 to table.capacity).foreach(_ => dispatcher.assign())
      registry.updateFreeSeats(table.id, 0)

  test("a player is assigned to a table with a free seat, and gets its address"):
    registerTable("table-1")
    dispatcher.assign() shouldBe Assignment.Assigned(registry.find("table-1").get)
    registry.find("table-1").map(t => (t.host, t.wsPort)) shouldBe Some(("localhost", 9001))

  test("a table is never given more players than it has seats"):
    registerTable("table-1")
    val results = Seq.fill(3)(dispatcher.assign())
    results.take(2).map(assignedTo) shouldBe Seq(Some("table-1"), Some("table-1"))
    assignedTo(results(2)) shouldBe None

  test("the players go to the first table with a free seat, and to the next one when it is full"):
    Seq("table-1", "table-2").foreach(registerTable)
    Seq.fill(4)(assignedTo(dispatcher.assign())) shouldBe Seq(Some("table-1"), Some("table-1"), Some("table-2"), Some("table-2"))

  test("when every table is full a new node is started, and the player is told to try again"):
    registerTable("table-1")
    fillAll()
    dispatcher.assign() shouldBe Assignment.Starting("table-2")
    provisioner.started.toList shouldBe List("table-2")

  test("while a node is starting no other node is started"):
    registerTable("table-1")
    fillAll()
    Seq.fill(3)(dispatcher.assign()).distinct shouldBe Seq(Assignment.Starting("table-2"))
    provisioner.started should have size 1

  test("when the node that was starting has registered, the players are sent there"):
    registerTable("table-1")
    fillAll()
    dispatcher.assign() shouldBe Assignment.Starting("table-2")
    registerTable("table-2")
    assignedTo(dispatcher.assign()) shouldBe Some("table-2")

  test("a node that does not register within the timeout is started again"):
    registerTable("table-1")
    fillAll()
    dispatcher.assign()
    now = t0.plus(startTimeout).plusSeconds(1)
    dispatcher.assign() shouldBe Assignment.Starting("table-2")
    provisioner.started.toList shouldBe List("table-2", "table-2")

  test("beyond the maximum number of tables the answer is 'full', at once, and no node is started"):
    Seq("table-1", "table-2", "table-3").foreach(registerTable)
    fillAll()
    val startedBefore = provisioner.started.size
    dispatcher.assign() shouldBe Assignment.Full
    provisioner.started should have size startedBefore

  test("a table that is registered but unavailable still counts as one of the tables"):
    val onlyOne = dispatcherFor(Seq("table-1"))
    registerTable("table-1")
    registry.markStatus("table-1", TableStatus.Unavailable)
    onlyOne.assign() shouldBe Assignment.Full
    provisioner.started shouldBe empty

  test("with no node that can be started the answer is 'full'"):
    dispatcherFor(Seq.empty).assign() shouldBe Assignment.Full

  test("if the node cannot be started the player is told, and the start is tried again at the next request"):
    provisioner.works = false
    dispatcher.assign() shouldBe Assignment.ActivationFailed("table-1")
    provisioner.works = true
    dispatcher.assign() shouldBe Assignment.Starting("table-1")
    provisioner.started.toList shouldBe List("table-1", "table-1")

  test("a seat given to a player who never arrives is available again after the hold time"):
    registerTable("table-1")
    Seq.fill(2)(dispatcher.assign())
    assignedTo(dispatcher.assign()) shouldBe None
    now = t0.plus(seatHold).plusSeconds(1)
    assignedTo(dispatcher.assign()) shouldBe Some("table-1")
