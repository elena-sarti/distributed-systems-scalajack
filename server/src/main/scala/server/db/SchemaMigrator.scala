package server.db

import java.nio.charset.StandardCharsets
import scala.util.Using

/** Brings the database schema up to date by running `db/schema.sql`.
 *
 * Repeatable by construction: the script only contains idempotent statements, so running it again is harmless. */
object SchemaMigrator:
  private val SchemaResource = "/db/schema.sql"

  def migrate(database: Database): Unit =
    val script = readScript()
    database.withConnection: connection =>
      connection.setAutoCommit(false) // all or nothing: a failure in the middle leaves no half-built schema
      Using.resource(connection.createStatement())(_.execute(script))
      connection.commit()

  private def readScript(): String =
    val stream = Option(getClass.getResourceAsStream(SchemaResource))
      .getOrElse(throw IllegalStateException(s"Resource $SchemaResource not found"))
    Using.resource(stream)(in => String(in.readAllBytes(), StandardCharsets.UTF_8))
