package server

import utils.TestExports.*
import scala.collection.mutable.ListBuffer

class TableAnnouncerTest extends AnyFunSuite with BeforeAndAfterEach:

  /** A lobby that records what it is told, and answers what the test decides. */
  private final class FakeGateway extends LobbyGateway:
    var registrationWorks = true
    var heartbeatAnswer   = HeartbeatResult.Acknowledged
    val registrations     = ListBuffer.empty[(String, String, Int)]
    val heartbeats        = ListBuffer.empty[(String, Int)]

    def register(id: String, host: String, wsPort: Int): Boolean =
      registrations += ((id, host, wsPort))
      registrationWorks

    def heartbeat(id: String, freeSeats: Int): HeartbeatResult =
      heartbeats += ((id, freeSeats))
      heartbeatAnswer

  private final class FakeSeats(var freeSeats: Int) extends SeatAvailability

  private var gateway: FakeGateway = _
  private var seats: FakeSeats     = _
  private var announcer: TableAnnouncer = _

  override def beforeEach(): Unit =
    gateway = FakeGateway()
    seats = FakeSeats(7)
    announcer = TableAnnouncer(gateway, "table-1", "localhost", 9091, seats)

  test("the first tick registers the table with the address the clients must use, then sends the heartbeat"):
    announcer.tick()
    gateway.registrations.toList shouldBe List(("table-1", "localhost", 9091))
    gateway.heartbeats.toList shouldBe List(("table-1", 7))

  test("the next ticks only send the heartbeat, with the seats that are free at that moment"):
    announcer.tick()
    seats.freeSeats = 4
    announcer.tick()
    gateway.registrations should have size 1
    gateway.heartbeats.toList shouldBe List(("table-1", 7), ("table-1", 4))

  test("if the lobby cannot be reached the table keeps trying to register, and sends no heartbeat meanwhile"):
    gateway.registrationWorks = false
    announcer.tick()
    announcer.tick()
    gateway.registrations should have size 2
    gateway.heartbeats shouldBe empty
    gateway.registrationWorks = true
    announcer.tick()
    gateway.heartbeats should have size 1

  test("if the lobby does not know the table any more (e.g. it restarted) the table registers again"):
    announcer.tick()
    gateway.heartbeatAnswer = HeartbeatResult.UnknownTable
    announcer.tick()
    gateway.heartbeatAnswer = HeartbeatResult.Acknowledged
    announcer.tick()
    gateway.registrations should have size 2

  test("a heartbeat that does not arrive is not a reason to register again"):
    announcer.tick()
    gateway.heartbeatAnswer = HeartbeatResult.Unreachable
    announcer.tick()
    announcer.tick()
    gateway.registrations should have size 1
    gateway.heartbeats should have size 3
