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

  test("a new user has the initial balance"):
    balances.balanceOf("Alice") shouldBe Some(BalanceRepository.InitialBalance)
    balances.balanceOf("Nobody") shouldBe None

  test("sitting down gives the balance; leaving saves it, and the next table finds it"):
    balances.sitDown("Alice", "table-1") shouldBe Right(100.0)
    balances.standUp("Alice", "table-1", 152.5) shouldBe true
    balances.sitDown("Alice", "table-2") shouldBe Right(152.5)

  test("a user sitting at a table cannot sit at another one"):
    balances.sitDown("Alice", "table-1")
    balances.sitDown("alice", "table-2") shouldBe Left(SitDownError.AlreadySeated("table-1"))

  test("sitting down again at the same table is allowed"):
    balances.sitDown("Alice", "table-1")
    balances.sitDown("Alice", "table-1") shouldBe Right(100.0)

  test("an unknown user cannot sit down"):
    balances.sitDown("Nobody", "table-1") shouldBe Left(SitDownError.UnknownUser)

  test("a table cannot save the balance of a user who is sitting elsewhere"):
    balances.sitDown("Alice", "table-1")
    balances.standUp("Alice", "table-2", 1000.0) shouldBe false
    balances.balanceOf("Alice") shouldBe Some(100.0)

  test("a table that starts again frees its users, and only its own"):
    balances.sitDown("Alice", "table-1")
    balances.sitDown("Bob", "table-2")
    balances.releaseTable("table-1")
    balances.sitDown("Alice", "table-3") shouldBe Right(100.0)
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
