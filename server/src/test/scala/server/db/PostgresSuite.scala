package server.db

import utils.TestExports.*
import scala.util.Using

/** Base of the tests that need a database: every suite has its own, and every test starts without the schema. */
abstract class PostgresSuite extends AnyFunSuite with BeforeAndAfterEach:
  protected val database: Database = TestPostgres.newDatabase()

  override def beforeEach(): Unit = execute("DROP TABLE IF EXISTS users")

  protected def execute(sql: String): Unit =
    database.withConnection(connection => Using.resource(connection.createStatement())(_.execute(sql)))
