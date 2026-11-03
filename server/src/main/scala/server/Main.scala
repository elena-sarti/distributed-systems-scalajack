package server

import com.sun.net.httpserver.HttpHandler
import protocol.LobbyApi
import server.auth.{LoginEndpoint, LoginService, Pbkdf2PasswordHasher, RegistrationEndpoint, RegistrationService, SessionTokens}
import server.lobby.{HeartbeatMonitor, TableHeartbeatEndpoint, TableRegistrationEndpoint, TableRegistry}
import server.db.{Database, DbConfig, PostgresBalanceRepository, PostgresUserRepository, SchemaMigrator}
import scala.util.control.NonFatal

object Main:
  def main(args: Array[String]): Unit =
    NodeConfig.fromEnv(sys.env) match
      case Left(error) => fail(s"Invalid configuration: $error") //fail fast: if the configuration is wrong, the process just fails
      case Right(config) =>
        val tokens = sessionTokens() // the lobby issues the session tokens, the tables check them
        // the lobby owns the users: their table must exist before the node accepts anything
        val registry = TableRegistry() // only used by the lobby: the tables that announced themselves
        val routes   = if config.role == NodeRole.Lobby then lobbyRoutes(prepareDatabase(), tokens, registry) else Map.empty
        // the lobby checks every period which tables have gone silent
        val monitor  = Option.when(config.role == NodeRole.Lobby)(silenceMonitor(registry, config.heartbeat))
        val health = HealthServer(config.role, config.port, routes)
        // a table node runs one table: its logic is driven by a single writer thread
        val logic    = Option.when(config.role == NodeRole.Table)(tableLogic(config.tableId))
        val table    = logic.map(SingleWriterHandler(_))
        val endpoint = table.map(TableEndpoint(config.wsPort, _, tokens))
        // ...and it tells the lobby that it exists and how many seats are free (if it knows where the lobby is)
        val announcer = for seats <- logic; url <- config.lobbyUrl yield heartbeatSender(config, url, seats)
        sys.addShutdownHook:
          // the table first: it finishes the events already queued and gives every player their money back;
          // closing the connections afterwards does not count as players leaving in the middle of a hand
          announcer.foreach(_.stop())
          monitor.foreach(_.stop())
          table.foreach(_.stop())
          endpoint.foreach(_.stop())
          health.stop()
        health.start()
        monitor.foreach(_.start())
        endpoint.foreach: e =>
          e.start()
          println(s"WebSocket endpoint listening on port ${config.wsPort}")
        announcer.foreach(_.start()) // only now the table can really accept the players the lobby will send
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

  /** Checks every heartbeat period which tables have not been heard from within the timeout. */
  private def silenceMonitor(registry: TableRegistry, heartbeat: HeartbeatConfig): PeriodicTask =
    val monitor = HeartbeatMonitor(registry, heartbeat.timeout)
    PeriodicTask("heartbeat-monitor", heartbeat.period)(() => { monitor.check(); () })

  /** Registers the table at the lobby and then sends a heartbeat every period (the first one at once). */
  private def heartbeatSender(config: NodeConfig, lobbyUrl: String, seats: SeatAvailability): PeriodicTask =
    val announcer = TableAnnouncer(HttpLobbyGateway(lobbyUrl), config.tableId, config.publicHost, config.advertisedWsPort, seats)
    PeriodicTask("table-announcer", config.heartbeat.period, runFirstNow = true)(() => announcer.tick())

  /** The HTTP API of the lobby, next to /health. */
  private def lobbyRoutes(database: Database, tokens: SessionTokens, registry: TableRegistry): Map[String, HttpHandler] =
    val users  = PostgresUserRepository(database)
    val hasher = Pbkdf2PasswordHasher()
    Map(
      LobbyApi.RegisterPath -> RegistrationEndpoint(RegistrationService(users, hasher)),
      LobbyApi.LoginPath    -> LoginEndpoint(LoginService(users, PostgresBalanceRepository(database), hasher, tokens)),
      LobbyApi.TableRegisterPath  -> TableRegistrationEndpoint(registry),
      LobbyApi.TableHeartbeatPath -> TableHeartbeatEndpoint(registry)
    )

  private def fail(message: String): Nothing =
    System.err.println(message)
    sys.exit(1)
