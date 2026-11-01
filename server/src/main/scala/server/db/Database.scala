package server.db

import java.sql.{Connection, DriverManager}
import scala.util.Using

/** Access point to the database: it hands out a connection for the time of one operation.
 *
 * No connection pool on purpose (KISS): only registration and login use the database, and they are rare.
 * If it ever becomes a bottleneck, only this class has to change. */
final class Database(config: DbConfig):

  /** Opens a connection, gives it to `use` and always closes it afterwards, even if `use` throws. */
  def withConnection[A](use: Connection => A): A =
    Using.resource(DriverManager.getConnection(config.url, config.user, config.password))(use)
