package server.db

import io.zonky.test.db.postgres.embedded.EmbeddedPostgres
import java.util.concurrent.atomic.AtomicInteger
import scala.util.Using

/** A real PostgreSQL for the tests, started on demand (no Docker needed) and shared by the whole test run.
 * Every test suite gets its own empty database, so suites running in parallel never see each other's data. */
object TestPostgres:
  private val AdminUser = "postgres"

  // lazy: started by the first suite that needs it; closed when the JVM ends
  private lazy val server =
    val postgres = EmbeddedPostgres.start()
    sys.addShutdownHook(postgres.close())
    postgres

  private val databases = AtomicInteger(0)

  def newDatabase(): Database =
    val name = s"test_${databases.incrementAndGet()}"
    Using.resource(server.getPostgresDatabase.getConnection): connection =>
      Using.resource(connection.createStatement())(_.execute(s"CREATE DATABASE $name"))
    Database(DbConfig(server.getJdbcUrl(AdminUser, name), AdminUser, AdminUser))
