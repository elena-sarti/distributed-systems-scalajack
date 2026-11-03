package server.lobby

import protocol.LobbyApi
import protocol.LobbyApi.AssignRequest
import server.auth.{SessionTokens, TestLobby}
import server.auth.TestLobby.capturedOutput
import utils.TestExports.*
import java.time.Instant
import java.util.concurrent.{Callable, ConcurrentLinkedQueue, CyclicBarrier, Executors, TimeUnit}

/** Many players asking for a seat at the same moment. Whatever the order in which the threads run, the
 * result must be the same: so these tests pass or fail by the code, not by luck; they are repeated many
 * times only to make a wrong code likely to be caught (a race may show up one time in a hundred). */
class TableDispatcherConcurrencyTest extends AnyFunSuite:

  /** Rounds of the main tests: at least 100, as the backlog asks. */
  private val Repetitions = 200

  /** A provisioner for the tests where no node must be started: if the dispatcher calls it, the test fails. */
  private object NeverStarts extends TableProvisioner:
    def start(tableId: String): Boolean = throw AssertionError(s"No node should be started, but $tableId was")

  /** Records the nodes it is asked to start. It is slow on purpose, which widens the time in which a second
   * thread could wrongly ask for another node. */
  private final class SlowProvisioner extends TableProvisioner:
    val started = ConcurrentLinkedQueue[String]()

    def start(tableId: String): Boolean =
      started.add(tableId)
      Thread.sleep(1)
      true

  /** Runs the task for `clients` threads that all start at the same moment (a barrier lets them go
   * together, so they really compete), and returns what each of them got. */
  private def concurrently[A](clients: Int)(task: Int => A): Seq[A] =
    val pool    = Executors.newFixedThreadPool(clients)
    val barrier = CyclicBarrier(clients)
    try
      val futures = (0 until clients).map: client =>
        val job: Callable[A] = () =>
          barrier.await()
          task(client)
        pool.submit(job)
      futures.map(_.get(10, TimeUnit.SECONDS)) // an exception in a client (e.g. a failed assertion) shows up here
    finally pool.shutdownNow()

  private def repeat(times: Int)(round: => Unit): Unit =
    (1 to times).foreach(n => withClue(s"Round $n: ")(round))

  private def assignedTables(results: Seq[Assignment]): Seq[String] =
    results.collect { case Assignment.Assigned(table) => table.id }

  /** A lobby with tables of the given capacity, already registered, and a dispatcher that cannot start nodes
   * (its pool is just those tables). */
  private def lobbyWith(capacity: Int, tables: String*): (TableRegistry, TableDispatcher) =
    val registry = TableRegistry(capacity)
    tables.foreach(registry.register(_, "localhost", 9091))
    (registry, TableDispatcher(registry, NeverStarts, pool = tables))

  test("with one seat and two players at the same moment, one is assigned and the other is refused (repeated)"):
    repeat(Repetitions):
      val (_, dispatcher) = lobbyWith(1, "table-1")
      val results         = concurrently(2)(_ => dispatcher.assign())
      assignedTables(results) shouldBe Seq("table-1")
      results.count(_ == Assignment.Full) shouldBe 1

  test("with 3 seats and 50 players at the same moment, exactly 3 are assigned, and the table is then full"):
    repeat(100):
      val (registry, dispatcher) = lobbyWith(3, "table-1")
      val results                = concurrently(50)(_ => dispatcher.assign())
      assignedTables(results) should have size 3
      registry.assignableSeats("table-1", Instant.now()) shouldBe Some(0)

  test("with two tables of 2 seats and 20 players, each table gets exactly 2"):
    repeat(100):
      val (_, dispatcher) = lobbyWith(2, "table-1", "table-2")
      val perTable        = assignedTables(concurrently(20)(_ => dispatcher.assign())).groupBy(identity).view.mapValues(_.size).toMap
      perTable shouldBe Map("table-1" -> 2, "table-2" -> 2)

  test("when everything is full and many players arrive together, one node is started, not one for each"):
    repeat(100):
      val registry = TableRegistry(capacity = 1)
      registry.register("table-1", "localhost", 9091)
      registry.updateFreeSeats("table-1", 0) // full
      val provisioner = SlowProvisioner()
      val dispatcher  = TableDispatcher(registry, provisioner, pool = Seq("table-1", "table-2", "table-3"))
      val results     = concurrently(20)(_ => dispatcher.assign())
      results.distinct shouldBe Seq(Assignment.Starting("table-2"))
      provisioner.started.toArray.toList shouldBe List("table-2")

  test("a heartbeat that arrives while players are assigned does not give a reserved seat back"):
    repeat(100):
      val (registry, dispatcher) = lobbyWith(1, "table-1")
      @volatile var running = true
      // the table keeps saying that its seat is free, as it does until the player really sits down
      val heartbeats = Thread(() => while running do registry.heartbeat("table-1", 1), "test-heartbeats")
      heartbeats.start()
      try assignedTables(concurrently(2)(_ => dispatcher.assign())) shouldBe Seq("table-1")
      finally
        running = false
        heartbeats.join()

  test("through the HTTP endpoint too: with one seat and two requests at the same moment, one gets 200 and the other 503"):
    val registry   = TableRegistry(capacity = 1)
    val tokens     = SessionTokens("a test secret that is long enough for HMAC".getBytes("UTF-8"))
    val dispatcher = TableDispatcher(registry, NeverStarts, pool = Seq("table-1"))
    val lobby      = TestLobby(Map(LobbyApi.AssignPath -> TableAssignmentEndpoint(dispatcher, tokens)))
    try
      // the endpoint logs every assignment: the output is captured only to keep the test report readable
      capturedOutput:
        repeat(100):
          registry.register("table-1", "localhost", 9091) // registering again gives a fresh table, with its seat free
          val statuses = concurrently(2): client =>
            lobby.post(LobbyApi.AssignPath, LobbyApi.encode(AssignRequest(tokens.issue(s"Player$client")))).statusCode()
          statuses.sorted shouldBe Seq(200, 503)
    finally lobby.stop()
