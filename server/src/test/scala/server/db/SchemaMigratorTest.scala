package server.db

import utils.TestExports.*
import java.sql.SQLException

class SchemaMigratorTest extends PostgresSuite:

  private def insertUser(username: String): Unit =
    execute(s"INSERT INTO users (username, password_hash) VALUES ('$username', 'hash')")

  private def countUsers(): Int =
    database.withConnection: connection =>
      val rows = connection.createStatement().executeQuery("SELECT count(*) FROM users")
      rows.next()
      rows.getInt(1)

  test("the migration creates the users table"):
    SchemaMigrator.migrate(database)
    insertUser("Alice")
    countUsers() shouldBe 1

  test("the migration can be run again and again without errors and without touching the data"):
    SchemaMigrator.migrate(database)
    insertUser("Alice")
    (1 to 3).foreach(_ => SchemaMigrator.migrate(database))
    countUsers() shouldBe 1

  test("the database refuses two users with the same username, whatever the case"):
    SchemaMigrator.migrate(database)
    insertUser("Alice")
    val error = intercept[SQLException](insertUser("alice"))
    error.getSQLState shouldBe "23505" // PostgreSQL code for "unique violation"

  test("on a database created before the balances, the migration adds them and keeps the users"):
    execute("CREATE TABLE users (id BIGSERIAL PRIMARY KEY, username VARCHAR(32) NOT NULL, " +
      "password_hash TEXT NOT NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT now())")
    insertUser("Alice")
    SchemaMigrator.migrate(database)
    PostgresBalanceRepository(database).balanceOf("Alice") shouldBe Some(BalanceRepository.InitialBalance)
    countUsers() shouldBe 1

  test("a database created when new users started with 100 gets the new default, and the stored balances stay"):
    execute("CREATE TABLE users (id BIGSERIAL PRIMARY KEY, username VARCHAR(32) NOT NULL, " +
      "password_hash TEXT NOT NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT now(), " +
      "balance NUMERIC(12, 2) NOT NULL DEFAULT 100, table_id TEXT)")
    insertUser("Alice") // registered under the old rule: she keeps what she has
    SchemaMigrator.migrate(database)
    insertUser("Bob")   // registered under the new rule
    val balances = PostgresBalanceRepository(database)
    balances.balanceOf("Alice") shouldBe Some(100.0)
    balances.balanceOf("Bob") shouldBe Some(0.0)

  test("nodes that start together and migrate at the same moment take turns, without errors"):
    val start  = java.util.concurrent.CountDownLatch(1)
    val errors = java.util.concurrent.ConcurrentLinkedQueue[Throwable]()
    val nodes = (1 to 4).map: _ =>
      Thread: () =>
        start.await()
        try SchemaMigrator.migrate(database)
        catch case error: Throwable => errors.add(error)
        ()
    nodes.foreach(_.start())
    start.countDown()
    nodes.foreach(_.join())
    errors.isEmpty shouldBe true
    insertUser("Alice")
    countUsers() shouldBe 1
