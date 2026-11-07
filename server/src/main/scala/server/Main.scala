package server

import com.sun.net.httpserver.HttpHandler
import protocol.LobbyApi
import server.auth.{LoginEndpoint, LoginService, Pbkdf2PasswordHasher, RegistrationEndpoint, RegistrationService, SessionTokens}
import server.lobby.{BackupExclusionEndpoint, BackupHeartbeatEndpoint, DockerTableProvisioner, HeartbeatMonitor, PrimaryStartEndpoint, PromotionArbiter, PromotionEndpoint, TableAssignmentEndpoint, TableDispatcher, TableLocationEndpoint, TableHeartbeatEndpoint, TableRegistrationEndpoint, TableRegistry}
import server.db.{Database, DbConfig, PostgresBalanceRepository, PostgresUserRepository, SchemaMigrator}
import server.liveness.{FailureDetector, LoggingFailureListener, ObservedLobbyGateway, Peer}
import server.replication.{BackupExcluder, BackupHeartbeatSender, BackupPromoter, BackupReplica, HttpReplicator, PrimaryHeartbeatEndpoint, ReplicaEndpoint, ReplicatedTable, Replicator, TableSnapshot, TableTakeover}
import java.nio.file.Path
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean
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
        // what a backup starts when it becomes the primary (F4): to be stopped at shutdown like the rest
        val stoppers = ConcurrentLinkedQueue[() => Unit]()
        // only a backup that knows the lobby can be promoted: the lobby is the third vote
        val promoter = Option.when(config.role == NodeRole.Backup)(config.lobbyUrl).flatten.map: url =>
          BackupPromoter(config.tableId, replica, peers, HttpLobbyGateway(url), takeOver(config, tokens, peers, stoppers))
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
        val replicator = Option.when(config.role == NodeRole.Table)(config.backupUrl).flatten.map(url => HttpReplicator(url))
        // a replicated table gets its epoch from the lobby before doing anything (F5), and waits while the table is
        // played elsewhere: before that it must not even free the players marked at the table in the database
        val epoch    = replicator.map(_ => startEpoch(config)).getOrElse(0L)
        // a table node runs one table: its logic is driven by a single writer thread
        val logic    = Option.when(config.role == NodeRole.Table)(tableLogic(config, replicator, epoch))
        val table    = logic.map(SingleWriterHandler(_))
        val endpoint = table.map(TableEndpoint(config.wsPort, _, tokens))
        val replicated = logic.collect { case primary: ReplicatedTable => primary }
        // a primary that has been replaced refuses every move and closes the connections: the clients go back to
        // the lobby, which sends them to the node that plays the table now
        val stepDown = stepDownOnce(replicated, endpoint)
        // ...and it tells the lobby that it exists and how many seats are free (if it knows where the lobby is)
        val announcer = for seats <- logic; url <- config.lobbyUrl yield
          heartbeatSender(config, observed(url, peers, () => replicated.map(_.currentEpoch)), seats, stepDown)
        // a primary that cannot reach its backup asks the lobby to go on without it (F5)
        val excluder   = for primary <- replicated; url <- config.lobbyUrl yield
          BackupExcluder(config.tableId, primary, peers, HttpLobbyGateway(url), stepDown)
        // every node that depends on others sends its heartbeats and checks who has gone silent
        val liveness = Option.when(config.role != NodeRole.Lobby)(livenessTask(config, peers, replicator, replicated, promoter, excluder))
        sys.addShutdownHook:
          // the table first: it finishes the events already queued and gives every player their money back;
          // closing the connections afterwards does not count as players leaving in the middle of a hand
          liveness.foreach(_.stop())
          stoppers.forEach(stop => stop()) // the table a backup has started, if any
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
  private def observed(lobbyUrl: String, peers: FailureDetector[Peer], epoch: () => Option[Long] = () => None): LobbyGateway =
    ObservedLobbyGateway(HttpLobbyGateway(lobbyUrl, epoch = epoch), peers)

  /** Asks the lobby, at every period until it agrees, to be the primary of the table, and returns the epoch granted.
   * Without a lobby there is no arbiter, so no failover: any epoch will do. */
  private def startEpoch(config: NodeConfig): Long = config.lobbyUrl match
    case None => 1L
    case Some(url) =>
      val lobby = HttpLobbyGateway(url)
      var answer = lobby.requestStart(config.tableId)
      var last: Option[StartAnswer] = None
      while !answer.isInstanceOf[StartAnswer.Granted] do
        if !last.contains(answer) then println(s"Table ${config.tableId}: waiting to be the primary ($answer)")
        last = Some(answer)
        Thread.sleep(config.heartbeat.period.toMillis)
        answer = lobby.requestStart(config.tableId)
      val StartAnswer.Granted(epoch) = answer: @unchecked
      println(s"Table ${config.tableId}: primary with epoch $epoch")
      epoch

  /** What a primary does when the lobby says it has been replaced, done once whoever notices it first. */
  private def stepDownOnce(table: Option[ReplicatedTable], endpoint: Option[TableEndpoint]): () => Unit =
    val done = AtomicBoolean(false)
    () =>
      if done.compareAndSet(false, true) then
        table.foreach(_.stepDown())
        endpoint.foreach(_.stop())

  /** Every period: the heartbeats of this node (a table to its backup, a backup to the lobby), then the check of
   * who has been silent for too long. The heartbeat of a table to the lobby is sent by the announcer. */
  private def livenessTask(config: NodeConfig, peers: FailureDetector[Peer], replicator: Option[HttpReplicator],
                           primary: Option[ReplicatedTable], promoter: Option[BackupPromoter],
                           excluder: Option[BackupExcluder]): PeriodicTask =
    val beats: () => Unit = config.role match
      case NodeRole.Backup =>
        val lobby = config.lobbyUrl.map(observed(_, peers))
        // once promoted it is the table, not the backup of it: it must not look like a backup to the lobby any more
        () => if !promoter.exists(_.isPromoted) then lobby.foreach(_.backupHeartbeat(config.tableId))
      case _ =>
        val sender = for backup <- replicator; table <- primary yield BackupHeartbeatSender(backup, table.currentEpoch, peers)
        () => sender.foreach(_.tick())
    // a backup that suspects its primary asks the lobby, at every period, to be promoted (see `BackupPromoter`), and
    // a primary that suspects its backup asks to go on without it (see `BackupExcluder`)
    PeriodicTask("failure-detector", config.heartbeat.period, runFirstNow = true): () =>
      beats()
      peers.check()
      promoter.foreach(_.tick())
      excluder.foreach(_.tick())

  /** The backup takes the place of the failed primary (F4): the table continues from the last state the primary
   * confirmed, on this node, which announces itself to the lobby as the table. The players of the primary are
   * still marked as sitting at it in the database, so this table must not release them as a fresh one does. */
  private def takeOver(config: NodeConfig, tokens: SessionTokens, peers: FailureDetector[Peer],
                       stoppers: ConcurrentLinkedQueue[() => Unit]): (Option[TableSnapshot], Long) => Unit = (snapshot, epoch) =>
    println(s"Table ${config.tableId}: this backup takes over as primary, epoch $epoch")
    val logic    = TableTakeover.resume(snapshot, DatabaseCashier(PostgresBalanceRepository(prepareDatabase()), config.tableId))
    val table    = SingleWriterHandler(logic)
    val endpoint = TableEndpoint(config.wsPort, table, tokens)
    endpoint.start()
    println(s"WebSocket endpoint listening on port ${config.wsPort}")
    // registering replaces the entry of the failed primary: from now on the lobby sends the players to this node
    val announcer = config.lobbyUrl.map(url => heartbeatSender(config, observed(url, peers, () => Some(epoch)), logic))
    announcer.foreach(_.start())
    val stop: () => Unit = () => { announcer.foreach(_.stop()); table.stop(); endpoint.stop() }
    stoppers.add(stop)
    ()

  /** Checks every heartbeat period which tables and backups have not been heard from within the timeout. */
  private def silenceMonitor(registry: TableRegistry, backups: FailureDetector[String], heartbeat: HeartbeatConfig): PeriodicTask =
    val monitor = HeartbeatMonitor(registry, heartbeat.timeout)
    PeriodicTask("heartbeat-monitor", heartbeat.period)(() => { monitor.check(); backups.check(); () })

  /** Registers the table at the lobby and then sends a heartbeat every period (the first one at once). */
  private def heartbeatSender(config: NodeConfig, lobby: LobbyGateway, seats: SeatAvailability,
                              onSuperseded: () => Unit = () => ()): PeriodicTask =
    val announcer = TableAnnouncer(lobby, config.tableId, config.publicHost, config.advertisedWsPort, seats, onSuperseded)
    PeriodicTask("table-announcer", config.heartbeat.period, runFirstNow = true)(() => announcer.tick())

  /** The dispatcher sends the players to the tables, and starts the nodes of the pool through Docker when
   * all the tables are full. */
  private def tableDispatcher(config: NodeConfig, registry: TableRegistry): TableDispatcher =
    // each table of the pool has its backup, named after it (see docker-compose.yml): it is started with the table
    val provisioner = DockerTableProvisioner(Path.of(config.dockerSocket), backupName = id => Some(s"$id-backup"))
    TableDispatcher(registry, provisioner, config.tablePool)

  /** The HTTP API of the lobby, next to /health. */
  private def lobbyRoutes(database: Database, tokens: SessionTokens, registry: TableRegistry,
                          dispatcher: TableDispatcher, backups: FailureDetector[String]): Map[String, HttpHandler] =
    val users    = PostgresUserRepository(database)
    val balances = PostgresBalanceRepository(database)
    val hasher  = Pbkdf2PasswordHasher()
    val arbiter = PromotionArbiter(registry, backups.forget) // a backup that becomes the primary is no longer watched as a backup
    Map(
      LobbyApi.RegisterPath -> RegistrationEndpoint(RegistrationService(users, hasher)),
      LobbyApi.LoginPath    -> LoginEndpoint(LoginService(users, balances, hasher, tokens)),
      // a replicated table tells its epoch: a primary that has been replaced is refused
      LobbyApi.TableRegisterPath  -> TableRegistrationEndpoint(registry, arbiter),
      LobbyApi.TableHeartbeatPath -> TableHeartbeatEndpoint(registry, arbiter),
      LobbyApi.BackupHeartbeatPath -> BackupHeartbeatEndpoint(backups),
      // the lobby is the arbiter of the configuration of the tables: promotions and exclusions share the epochs
      LobbyApi.PromotionPath       -> PromotionEndpoint(arbiter),
      LobbyApi.ExclusionPath       -> BackupExclusionEndpoint(arbiter),
      LobbyApi.StartPath           -> PrimaryStartEndpoint(arbiter),
      LobbyApi.LocatePath -> TableLocationEndpoint(balances, registry, tokens), // where is my table now? (after a failover)
      LobbyApi.AssignPath         -> TableAssignmentEndpoint(dispatcher, tokens) // only says where to play: the moves go to the table
    )

  private def fail(message: String): Nothing =
    System.err.println(message)
    sys.exit(1)
