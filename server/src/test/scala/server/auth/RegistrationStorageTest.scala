package server.auth

import utils.TestExports.*
import server.db.{PostgresSuite, PostgresUserRepository, SchemaMigrator}
import scala.util.Using

/** The registration on the real database: what ends up in the `users` table. */
class RegistrationStorageTest extends PostgresSuite:

  private val password = "very secret password"

  var registration: RegistrationService = _

  override def beforeEach(): Unit =
    super.beforeEach()
    SchemaMigrator.migrate(database)
    registration = RegistrationService(PostgresUserRepository(database), Pbkdf2PasswordHasher(iterations = 1_000))

  /** Every column of every row of the users table, as text. */
  private def everyStoredValue(): List[String] =
    database.withConnection: connection =>
      Using.resource(connection.createStatement()): statement =>
        Using.resource(statement.executeQuery("SELECT * FROM users")): rows =>
          val columns = rows.getMetaData.getColumnCount
          Iterator.continually(rows.next()).takeWhile(identity)
            .flatMap(_ => (1 to columns).map(i => Option(rows.getString(i)).getOrElse("")))
            .toList

  test("no column of the database contains the password"):
    registration.register("Alice", password).isRight shouldBe true
    val stored = everyStoredValue()
    stored.contains("Alice") shouldBe true // the row is there...
    stored.exists(_.contains(password)) shouldBe false // ...but the password is not

  test("a duplicate username is refused by the database, whatever the case"):
    registration.register("Alice", password)
    registration.register("ALICE", "another password") shouldBe Left(RegistrationError.UsernameTaken)
