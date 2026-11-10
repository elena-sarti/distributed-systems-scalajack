package server.db

import java.nio.charset.StandardCharsets
import scala.util.Using

/** Brings the database schema up to date by running `db/schema.sql`. The script only has idempotent statements, so running
 * it again is harmless. */
object SchemaMigrator:
  private val SchemaResource = "/db/schema.sql"
  private val MigrationLock  = 7_340_217L // any number, as long as every node uses the same one

  def migrate(database: Database): Unit =
    val script = readScript()
    database.withConnection: connection =>
      connection.setAutoCommit(false) // all or nothing: a failure in the middle leaves no half-built schema
      // the lobby and the tables migrate when they start, possibly at the same moment: they take turns on
      // this lock, so two migrations never run together
      Using.resource(connection.createStatement())(_.execute(s"SELECT pg_advisory_xact_lock($MigrationLock)"))
      Using.resource(connection.createStatement())(_.execute(script))
      connection.commit()

  private def readScript(): String =
    val stream = Option(getClass.getResourceAsStream(SchemaResource))
      .getOrElse(throw IllegalStateException(s"Resource $SchemaResource not found"))
    Using.resource(stream)(in => String(in.readAllBytes(), StandardCharsets.UTF_8))
