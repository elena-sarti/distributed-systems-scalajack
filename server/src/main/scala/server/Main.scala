package server

import server.db.{Database, DbConfig, SchemaMigrator}
import scala.util.control.NonFatal

object Main:
  def main(args: Array[String]): Unit =
    NodeConfig.fromEnv(sys.env) match
      case Left(error) => fail(s"Invalid configuration: $error") //fail fast: if the configuration is wrong, the process just fails
      case Right(config) =>
        // the lobby owns the users: their table must exist before the node accepts anything
        if config.role == NodeRole.Lobby then prepareDatabase()
        val health = HealthServer(config.role, config.port)
        // a table node runs one table: its logic is driven by a single writer thread
        val table    = Option.when(config.role == NodeRole.Table)(SingleWriterHandler(TableHandler()))
        val endpoint = table.map(TableEndpoint(config.wsPort, _))
        sys.addShutdownHook:
          endpoint.foreach(_.stop()) // first stop the network, so no new events arrive...
          table.foreach(_.stop())    // ...then let the table finish the events already queued
          health.stop()
        health.start()
        endpoint.foreach: e =>
          e.start()
          println(s"WebSocket endpoint listening on port ${config.wsPort}")
        println(s"ScalaJack node started: role=${config.role}, port=${config.port}")
        Thread.currentThread().join()

  /** Reads the database configuration and brings the schema up to date. As for the node configuration, a
   * wrong configuration or a database that cannot be reached stops the process: no point in running without users. */
  private def prepareDatabase(): Unit =
    DbConfig.fromEnv(sys.env) match
      case Left(error) => fail(s"Invalid database configuration: $error")
      case Right(dbConfig) =>
        try
          SchemaMigrator.migrate(Database(dbConfig))
          println("Database schema is up to date")
        catch case NonFatal(error) => fail(s"Cannot prepare the database: ${error.getMessage}")

  private def fail(message: String): Nothing =
    System.err.println(message)
    sys.exit(1)
