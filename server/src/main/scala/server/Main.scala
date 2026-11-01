package server

import com.sun.net.httpserver.HttpHandler
import protocol.LobbyApi
import server.auth.{LoginEndpoint, LoginService, Pbkdf2PasswordHasher, RegistrationEndpoint, RegistrationService, SessionTokens}
import server.db.{Database, DbConfig, PostgresBalanceRepository, PostgresUserRepository, SchemaMigrator}
import scala.util.control.NonFatal

object Main:
  def main(args: Array[String]): Unit =
    NodeConfig.fromEnv(sys.env) match
      case Left(error) => fail(s"Invalid configuration: $error") //fail fast: if the configuration is wrong, the process just fails
      case Right(config) =>
        val tokens = sessionTokens() // the lobby issues the session tokens, the tables check them
        // the lobby owns the users: their table must exist before the node accepts anything
        val routes = if config.role == NodeRole.Lobby then lobbyRoutes(prepareDatabase(), tokens) else Map.empty
        val health = HealthServer(config.role, config.port, routes)
        // a table node runs one table: its logic is driven by a single writer thread
        val table    = Option.when(config.role == NodeRole.Table)(SingleWriterHandler(tableLogic(config.tableId)))
        val endpoint = table.map(TableEndpoint(config.wsPort, _, tokens))
        sys.addShutdownHook:
          // the table first: it finishes the events already queued and gives every player their money back;
          // closing the connections afterwards does not count as players leaving in the middle of a hand
          table.foreach(_.stop())
          endpoint.foreach(_.stop())
          health.stop()
        health.start()
        endpoint.foreach: e =>
          e.start()
          println(s"WebSocket endpoint listening on port ${config.wsPort}")
        println(s"ScalaJack node started: role=${config.role}, port=${config.port}")
        Thread.currentThread().join()

  /** Reads the database configuration and brings the schema up to date. As for the node configuration, a
   * wrong configuration or a database that cannot be reached stops the process: no point in running without users. */
  private def prepareDatabase(): Database =
    DbConfig.fromEnv(sys.env) match
      case Left(error) => fail(s"Invalid database configuration: $error")
      case Right(dbConfig) =>
        val database = Database(dbConfig)
        try
          SchemaMigrator.migrate(database)
          println("Database schema is up to date")
          database
        catch case NonFatal(error) => fail(s"Cannot prepare the database: ${error.getMessage}")

  /** The tokens are signed with a secret shared by lobby and tables: a node without it cannot work. */
  private def sessionTokens(): SessionTokens =
    SessionTokens.fromEnv(sys.env).fold(error => fail(s"Invalid session configuration: $error"), identity)

  /** The table keeps the players' balances in the database. When it starts, nobody can be sitting at it:
   * the players still marked there were left by a crash, and are freed. */
  private def tableLogic(tableId: String): TableHandler =
    val balances = PostgresBalanceRepository(prepareDatabase())
    balances.releaseTable(tableId)
    TableHandler(cashier = DatabaseCashier(balances, tableId))

  /** The HTTP API of the lobby, next to /health. */
  private def lobbyRoutes(database: Database, tokens: SessionTokens): Map[String, HttpHandler] =
    val users  = PostgresUserRepository(database)
    val hasher = Pbkdf2PasswordHasher()
    Map(
      LobbyApi.RegisterPath -> RegistrationEndpoint(RegistrationService(users, hasher)),
      LobbyApi.LoginPath    -> LoginEndpoint(LoginService(users, PostgresBalanceRepository(database), hasher, tokens))
    )

  private def fail(message: String): Nothing =
    System.err.println(message)
    sys.exit(1)
