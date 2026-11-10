package server.db

import utils.TestExports.*
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}
import scala.jdk.CollectionConverters.*

class PostgresBalanceRepositoryTest extends PostgresSuite:

  private var balances: BalanceRepository = _

  override def beforeEach(): Unit =
    super.beforeEach()
    SchemaMigrator.migrate(database)
    val users = PostgresUserRepository(database)
    List("Alice", "Bob").foreach(users.create(_, "hash"))
    balances = PostgresBalanceRepository(database)

  private val Initial = BalanceRepository.InitialBalance

  test("a new user has no money"):
    BalanceRepository.InitialBalance shouldBe 0.0
    balances.balanceOf("Alice") shouldBe Some(0.0)
    balances.balanceOf("Nobody") shouldBe None

  test("the table where a user is sitting is known, ignoring the case, until they leave"):
    balances.tableOf("Alice") shouldBe None
    balances.sitDown("Alice", "table-1")
    balances.tableOf("alice") shouldBe Some("table-1")
    balances.standUp("Alice", "table-1", Initial)
    balances.tableOf("Alice") shouldBe None
    balances.tableOf("Nobody") shouldBe None

  test("sitting down gives the balance; leaving saves it, and the next table finds it"):
    balances.sitDown("Alice", "table-1") shouldBe Right(Initial)
    balances.standUp("Alice", "table-1", 152.5) shouldBe true
    balances.sitDown("Alice", "table-2") shouldBe Right(152.5)

  test("a user sitting at a table cannot sit at another one"):
    balances.sitDown("Alice", "table-1")
    balances.sitDown("alice", "table-2") shouldBe Left(SitDownError.AlreadySeated("table-1"))

  test("sitting down again at the same table is allowed"):
    balances.sitDown("Alice", "table-1")
    balances.sitDown("Alice", "table-1") shouldBe Right(Initial)

  test("an unknown user cannot sit down"):
    balances.sitDown("Nobody", "table-1") shouldBe Left(SitDownError.UnknownUser)

  test("a table cannot save the balance of a user who is sitting elsewhere"):
    balances.sitDown("Alice", "table-1")
    balances.standUp("Alice", "table-2", 1000.0) shouldBe false
    balances.balanceOf("Alice") shouldBe Some(Initial)

  test("a table that starts again frees its users, and only its own"):
    balances.sitDown("Alice", "table-1")
    balances.sitDown("Bob", "table-2")
    balances.releaseTable("table-1")
    balances.sitDown("Alice", "table-3") shouldBe Right(Initial)
    balances.sitDown("Bob", "table-3") shouldBe Left(SitDownError.AlreadySeated("table-2"))

  test("when many tables try to seat the same user at once, exactly one succeeds"):
    val start   = CountDownLatch(1)
    val results = ConcurrentLinkedQueue[Either[SitDownError, Double]]()
    val tables = (1 to 8).map: i =>
      Thread: () =>
        start.await()
        results.add(balances.sitDown("Alice", s"table-$i"))
        ()
    tables.foreach(_.start())
    start.countDown()
    tables.foreach(_.join())
    results.asScala.count(_.isRight) shouldBe 1

  // ---- deposit and withdrawal ----

  test("a deposit adds to the balance, with cents, and the user is found ignoring the case"):
    balances.deposit("Alice", 100.0) shouldBe Right(100.0)
    balances.deposit("alice", 12.5) shouldBe Right(112.5)
    balances.balanceOf("Alice") shouldBe Some(112.5)

  test("a withdrawal takes from the balance, down to zero"):
    balances.deposit("Alice", 100.0)
    balances.withdraw("Alice", 30.0) shouldBe Right(70.0)
    balances.withdraw("Alice", 70.0) shouldBe Right(0.0)
    balances.balanceOf("Alice") shouldBe Some(0.0)

  test("a withdrawal of more than the balance is refused and changes nothing"):
    balances.deposit("Alice", 100.0)
    balances.withdraw("Alice", 100.5) shouldBe Left(FundsError.InsufficientFunds)
    balances.balanceOf("Alice") shouldBe Some(100.0)

  test("an unknown user can neither deposit nor withdraw"):
    balances.deposit("Nobody", 10.0) shouldBe Left(FundsError.UnknownUser)
    balances.withdraw("Nobody", 10.0) shouldBe Left(FundsError.UnknownUser)

  test("while a user is sitting at a table their money cannot be moved, and nothing changes"):
    balances.deposit("Alice", 100.0)
    balances.sitDown("Alice", "table-1")
    balances.deposit("Alice", 50.0) shouldBe Left(FundsError.Seated("table-1"))
    balances.withdraw("Alice", 50.0) shouldBe Left(FundsError.Seated("table-1"))
    balances.standUp("Alice", "table-1", 130.0) // what the table gives back is not touched by the refused calls
    balances.balanceOf("Alice") shouldBe Some(130.0)

  test("after standing up the user can move the money the table has given back"):
    balances.sitDown("Alice", "table-1")
    balances.standUp("Alice", "table-1", 0.0)
    balances.deposit("Alice", 20.0) shouldBe Right(20.0)

  test("when two withdrawals of more than half the balance arrive at once, exactly one succeeds"):
    balances.deposit("Alice", 100.0)
    val start   = CountDownLatch(1)
    val results = ConcurrentLinkedQueue[Either[FundsError, Double]]()
    val clients = (1 to 2).map: _ =>
      Thread: () =>
        start.await()
        results.add(balances.withdraw("Alice", 80.0))
        ()
    clients.foreach(_.start())
    start.countDown()
    clients.foreach(_.join())
    results.asScala.count(_.isRight) shouldBe 1
    balances.balanceOf("Alice") shouldBe Some(20.0) // never negative, never withdrawn twice

  test("many deposits at once are all counted"):
    val start   = CountDownLatch(1)
    val clients = (1 to 8).map: _ =>
      Thread: () =>
        start.await()
        balances.deposit("Alice", 10.0)
        ()
    clients.foreach(_.start())
    start.countDown()
    clients.foreach(_.join())
    balances.balanceOf("Alice") shouldBe Some(80.0)
