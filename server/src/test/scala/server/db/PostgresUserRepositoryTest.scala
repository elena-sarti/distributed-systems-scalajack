package server.db

import utils.TestExports.*
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}
import scala.jdk.CollectionConverters.*

class PostgresUserRepositoryTest extends PostgresSuite:

  private var users: UserRepository = _

  override def beforeEach(): Unit =
    super.beforeEach()
    SchemaMigrator.migrate(database) // a fresh schema for every test
    users = PostgresUserRepository(database)

  test("a created user can be found, with the hash that was stored"):
    val created = users.create("Alice", "hash-1").toOption.get
    users.findByUsername("Alice") shouldBe Some(created)
    created.passwordHash shouldBe "hash-1"

  test("every user gets a different id"):
    val ids = List("Alice", "Bob", "Carol").map(users.create(_, "hash").toOption.get.id)
    ids.distinct.size shouldBe 3

  test("a username already taken is refused, and the first user is left untouched"):
    users.create("Alice", "hash-1")
    users.create("Alice", "hash-2") shouldBe Left(CreateUserError.UsernameTaken)
    users.findByUsername("Alice").map(_.passwordHash) shouldBe Some("hash-1")

  test("usernames that differ only by case are the same username"):
    users.create("Alice", "hash")
    users.create("ALICE", "hash") shouldBe Left(CreateUserError.UsernameTaken)
    users.findByUsername("aLiCe").map(_.username) shouldBe Some("Alice") // found, with the spelling used at registration

  test("an unknown user is not found"):
    users.findByUsername("Nobody") shouldBe None

  test("a name with SQL in it is just a name"):
    val tricky = "Bob'; DROP TABLE users; --"
    users.create(tricky, "hash").isRight shouldBe true
    users.findByUsername(tricky).map(_.username) shouldBe Some(tricky)
    users.findByUsername("Nobody") shouldBe None // the table is still there

  test("when many clients register the same username at once, only one succeeds"):
    val start   = CountDownLatch(1)
    val results = ConcurrentLinkedQueue[Either[CreateUserError, User]]()
    val clients = (1 to 8).map: i =>
      Thread: () =>
        start.await()
        results.add(users.create("Alice", s"hash-$i"))
        ()
    clients.foreach(_.start())
    start.countDown()
    clients.foreach(_.join())
    val all = results.asScala.toList
    all.count(_.isRight) shouldBe 1
    all.count(_ == Left(CreateUserError.UsernameTaken)) shouldBe 7
