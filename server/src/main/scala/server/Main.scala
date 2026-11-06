package server

import com.sun.net.httpserver.HttpHandler
import protocol.LobbyApi
import server.auth.{LoginEndpoint, LoginService, Pbkdf2PasswordHasher, RegistrationEndpoint, RegistrationService, SessionTokens}
import server.lobby.{BackupHeartbeatEndpoint, DockerTableProvisioner, HeartbeatMonitor, TableAssignmentEndpoint, TableDispatcher, TableHeartbeatEndpoint, TableRegistrationEndpoint, TableRegistry}
import server.db.{Database, DbConfig, PostgresBalanceRepository, PostgresUserRepository, SchemaMigrator}
import server.liveness.{FailureDetector, LoggingFailureListener, ObservedLobbyGateway, Peer}
import server.replication.{BackupHeartbeatSender, BackupReplica, HttpReplicator, PrimaryHeartbeatEndpoint, ReplicaEndpoint, ReplicatedTable, Replicator}
import java.nio.file.Path
import scala.util.control.NonFatal

object Main:
  def main(args: Array[String]): Unit =
    NodeConfig.fromEnv(sys.env) match
      case Left(error) => fail(s"Invalid configuration: $error") //fail fast: if the configuration is wrong, the process just fails
      case Right(config) =>
        val tokens = sessionTokens() // the lobby issues the session tokens, the tables check them
        // the lobby owns the users: their table must exist before the node accepts anything
        val registry = TableRegistry() // only used by the lobby: the tables that announced themselves
        // failure detection (F3): the lobby watches the backups (the primaries are watched by the registry),
        // a table node watches the lobby and its backup, a backup node watches the lobby and its primary
        val backups  = FailureDetector[String](config.heartbeat.timeout, LoggingFailureListener("lobby", id => s"the backup of table $id"))
        val peers    = FailureDetector[Peer](config.heartbeat.timeout, LoggingFailureListener(s"${config.role.toString.toLowerCase} ${config.tableId}"))
        val replica  = BackupReplica() // only used by the backup: the last state sent by its table
        val routes   = config.role match
          case NodeRole.Lobby  => lobbyRoutes(prepareDatabase(), tokens, registry, tableDispatcher(config, registry), backups)
          case NodeRole.Backup => Map(
            ReplicaEndpoint.Path          -> ReplicaEndpoint(replica),
            PrimaryHeartbeatEndpoint.Path -> PrimaryHeartbeatEndpoint(replica, peers) // the primary's heartbeats are its signs of life
          )
          case NodeRole.Table  => Map.empty
        // the lobby checks every period which tables and backups have gone silent
        val monitor  = Option.when(config.role == NodeRole.Lobby)(silenceMonitor(registry, backups, config.heartbeat))
        val health = HealthServer(config.role, config.port, routes)
        // the start time orders the primaries: a node that starts again is newer than the one that crashed
        val epoch      = System.currentTimeMillis()
        val replicator = Option.when(config.role == NodeRole.Table)(config.backupUrl).flatten.map(url => HttpReplicator(url))
        // a table node runs one table: its logic is driven by a single writer thread
        val logic    = Option.when(config.role == NodeRole.Table)(tableLogic(config, replicator, epoch))
        val table    = logic.map(SingleWriterHandler(_))
        val endpoint = table.map(TableEndpoint(config.wsPort, _, tokens))
        // ...and it tells the lobby that it exists and how many seats are free (if it knows where the lobby is)
        val announcer = for seats <- logic; url <- config.lobbyUrl yield heartbeatSender(config, observed(url, peers), seats)
        // every node that depends on others sends its heartbeats and checks who has gone silent
        val liveness = Option.when(config.role != NodeRole.Lobby)(livenessTask(config, peers, replicator, epoch))
        sys.addShutdownHook:
          // the table first: it finishes the events already queued and gives every player their money back;
          // closing the connections afterwards does not count as players leaving in the middle of a hand
          liveness.foreach(_.stop())
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
        watchedPeers(config).foreach(peers.watch) // from now on each of them has one timeout to show up
        announcer.foreach(_.start()) // only now the table can really accept the players the lobby will send
        liveness.foreach(_.start())
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
   * the players still marked there were left by a crash, and are freed. With a backup, every change of the
   * table is stored there before the players see it. */
  private def tableLogic(config: NodeConfig, replicator: Option[Replicator], epoch: Long): SessionHandler & SeatAvailability =
    val balances = PostgresBalanceRepository(prepareDatabase())
    balances.releaseTable(config.tableId)
    val cashier = DatabaseCashier(balances, config.tableId)
    replicator match
      case None    => TableHandler(cashier = cashier)
      case Some(r) => ReplicatedTable(cashier, r, epoch)

  /** The peers a node must hear from, according to its role and to what it knows about the others. */
  private def watchedPeers(config: NodeConfig): Seq[Peer] =
    val lobby = config.lobbyUrl.map(_ => Peer.Lobby).toSeq
    config.role match
      case NodeRole.Table  => lobby ++ config.backupUrl.map(_ => Peer.Backup)
      case NodeRole.Backup => Peer.Primary +: lobby
      case NodeRole.Lobby  => Seq.empty // it watches by table id: see `silenceMonitor`

  /** The lobby seen through the detector: every answer of the lobby is a sign of life. */
  private def observed(lobbyUrl: String, peers: FailureDetector[Peer]): LobbyGateway =
    ObservedLobbyGateway(HttpLobbyGateway(lobbyUrl), peers)

  /** Every period: the heartbeats of this node (a table to its backup, a backup to the lobby), then the check of
   * who has been silent for too long. The heartbeat of a table to the lobby is sent by the announcer. */
  private def livenessTask(config: NodeConfig, peers: FailureDetector[Peer], replicator: Option[HttpReplicator], epoch: Long): PeriodicTask =
    val beats: () => Unit = config.role match
      case NodeRole.Backup =>
        val lobby = config.lobbyUrl.map(observed(_, peers))
        () => lobby.foreach(_.backupHeartbeat(config.tableId))
      case _ =>
        val sender = replicator.map(BackupHeartbeatSender(_, epoch, peers))
        () => sender.foreach(_.tick())
    PeriodicTask("failure-detector", config.heartbeat.period, runFirstNow = true)(() => { beats(); peers.check(); () })

  /** Checks every heartbeat period which tables and backups have not been heard from within the timeout. */
  private def silenceMonitor(registry: TableRegistry, backups: FailureDetector[String], heartbeat: HeartbeatConfig): PeriodicTask =
    val monitor = HeartbeatMonitor(registry, heartbeat.timeout)
    PeriodicTask("heartbeat-monitor", heartbeat.period)(() => { monitor.check(); backups.check(); () })

  /** Registers the table at the lobby and then sends a heartbeat every period (the first one at once). */
  private def heartbeatSender(config: NodeConfig, lobby: LobbyGateway, seats: SeatAvailability): PeriodicTask =
    val announcer = TableAnnouncer(lobby, config.tableId, config.publicHost, config.advertisedWsPort, seats)
    PeriodicTask("table-announcer", config.heartbeat.period, runFirstNow = true)(() => announcer.tick())

  /** The dispatcher sends the players to the tables, and starts the nodes of the pool through Docker when
   * all the tables are full. */
  private def tableDispatcher(config: NodeConfig, registry: TableRegistry): TableDispatcher =
    TableDispatcher(registry, DockerTableProvisioner(Path.of(config.dockerSocket)), config.tablePool)

  /** The HTTP API of the lobby, next to /health. */
  private def lobbyRoutes(database: Database, tokens: SessionTokens, registry: TableRegistry,
                          dispatcher: TableDispatcher, backups: FailureDetector[String]): Map[String, HttpHandler] =
    val users  = PostgresUserRepository(database)
    val hasher = Pbkdf2PasswordHasher()
    Map(
      LobbyApi.RegisterPath -> RegistrationEndpoint(RegistrationService(users, hasher)),
      LobbyApi.LoginPath    -> LoginEndpoint(LoginService(users, PostgresBalanceRepository(database), hasher, tokens)),
      LobbyApi.TableRegisterPath  -> TableRegistrationEndpoint(registry),
      LobbyApi.TableHeartbeatPath -> TableHeartbeatEndpoint(registry),
      LobbyApi.BackupHeartbeatPath -> BackupHeartbeatEndpoint(backups),
      LobbyApi.AssignPath         -> TableAssignmentEndpoint(dispatcher, tokens) // only says where to play: the moves go to the table
    )

  private def fail(message: String): Nothing =
    System.err.println(message)
    sys.exit(1)
