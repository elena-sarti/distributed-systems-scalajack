package server

import server.lobby.TableProvisioner
import java.time.Duration

/** @param tableId   the name of the table run by a table node: it marks the players sitting there in the
 *                  database, so it must stay the same when the node starts again.
 * @param lobbyUrl  where a table node finds the lobby; without it the node does not announce itself.
 * @param publicHost the address clients use to reach a table node (not always the one the node sees).
 * @param publicWsPort the WebSocket port clients use, if it is not the node's own (e.g. a port published by Docker).
 * @param heartbeat how often the tables say they are alive and how long the lobby waits for them.
 * @param tablePool the ids of the tables the lobby may start, in the order it starts them: how many they are
 *                  is the maximum number of tables. Used by the lobby.
 * @param dockerSocket the Unix socket of the Docker daemon, used by the lobby to start the tables.
 * @param backupUrl where a table node finds its backup; without it the table is not replicated.
 * @param returnWindow how long a backup that takes over its table waits for the players of the failed primary to
 *                     come back to their seats.
 */
final case class NodeConfig(role: NodeRole, port: Int, wsPort: Int = NodeConfig.DefaultWsPort,
                            tableId: String = NodeConfig.DefaultTableId,
                            lobbyUrl: Option[String] = None,
                            publicHost: String = NodeConfig.DefaultPublicHost,
                            publicWsPort: Option[Int] = None,
                            heartbeat: HeartbeatConfig = HeartbeatConfig.Default,
                            tablePool: Seq[String] = Seq.empty,
                            dockerSocket: String = NodeConfig.DefaultDockerSocket,
                            backupUrl: Option[String] = None,
                            returnWindow: Duration = NodeConfig.DefaultReturnWindow):
  /** The WebSocket port to hand out to the clients. */
  def advertisedWsPort: Int = publicWsPort.getOrElse(wsPort)

/** Times of the heartbeats. Lobby and tables must agree: the timeout is a few periods, so that one lost
 * heartbeat does not make a table unavailable. */
final case class HeartbeatConfig(period: Duration, timeout: Duration)

object HeartbeatConfig:
  val Default: HeartbeatConfig = HeartbeatConfig(Duration.ofSeconds(2), Duration.ofSeconds(6))

  def fromEnv(env: Map[String, String]): Either[String, HeartbeatConfig] =
    for
      period  <- seconds("HEARTBEAT_PERIOD_SECONDS", env, Default.period)
      timeout <- seconds("HEARTBEAT_TIMEOUT_SECONDS", env, Default.timeout)
      _       <- Either.cond(timeout.compareTo(period) > 0, (), "HEARTBEAT_TIMEOUT_SECONDS must be longer than HEARTBEAT_PERIOD_SECONDS")
    yield HeartbeatConfig(period, timeout)

  private def seconds(name: String, env: Map[String, String], default: Duration): Either[String, Duration] =
    env.get(name).fold(Right(default)): s =>
      s.toLongOption.filter(_ >= 1).map(Duration.ofSeconds).toRight(s"Invalid $name '$s'")

object NodeConfig:
  val DefaultPort = 8080
  val DefaultWsPort = 8090 //webSocket port: it's easier to use two different ports for the health HTTP and for the WebSocket
  val DefaultTableId = "table"
  val DefaultPublicHost = "localhost"
  val DefaultDockerSocket = "/var/run/docker.sock"
  val DefaultReturnWindow: Duration = Duration.ofSeconds(30)

  /** Pure function: takes the environment as a parameter, so it is trivially testable. */
  def fromEnv(env: Map[String, String]): Either[String, NodeConfig] =
    for
      rawRole <- env.get("NODE_ROLE").toRight("NODE_ROLE is not set")
      role    <- NodeRole.parse(rawRole)
      port    <- parsePort("NODE_PORT", env, DefaultPort)
      wsPort  <- parsePort("NODE_WS_PORT", env, DefaultWsPort)
      tableId  = env.get("TABLE_ID").map(_.trim).filter(_.nonEmpty).getOrElse(DefaultTableId)
      lobbyUrl <- parseUrl("LOBBY_URL", env)
      publicHost = env.get("TABLE_HOST").map(_.trim).filter(_.nonEmpty).getOrElse(DefaultPublicHost)
      publicWsPort <- env.get("TABLE_PUBLIC_WS_PORT").fold[Either[String, Option[Int]]](Right(None))(_ => parsePort("TABLE_PUBLIC_WS_PORT", env, wsPort).map(Some(_)))
      heartbeat <- HeartbeatConfig.fromEnv(env)
      pool <- parsePool(env)
      dockerSocket = env.get("DOCKER_SOCKET").map(_.trim).filter(_.nonEmpty).getOrElse(DefaultDockerSocket)
      backupUrl <- parseUrl("BACKUP_URL", env)
      returnWindow <- parseSeconds("RETURN_WINDOW_SECONDS", env, DefaultReturnWindow)
    yield NodeConfig(role, port, wsPort, tableId, lobbyUrl, publicHost, publicWsPort, heartbeat, pool, dockerSocket, backupUrl, returnWindow)

  private def parseSeconds(name: String, env: Map[String, String], default: Duration): Either[String, Duration] =
    env.get(name).fold(Right(default)): s =>
      s.toLongOption.filter(_ >= 1).map(Duration.ofSeconds).toRight(s"Invalid $name '$s'")

  private def parsePort(name: String, env: Map[String, String], default: Int): Either[String, Int] =
    env.get(name).fold(Right(default)): s =>
      s.toIntOption.filter(p => p >= 1 && p <= 65535).toRight(s"Invalid $name '$s'")

  /** Absent is fine (e.g. the node just does not announce itself); present, it must be an http(s) address. */
  private def parseUrl(name: String, env: Map[String, String]): Either[String, Option[String]] =
    env.get(name).fold(Right(None)): raw =>
      val url = raw.trim
      Either.cond(url.startsWith("http://") || url.startsWith("https://"), Some(url), s"Invalid $name '$raw'")

  /** A comma separated list of table ids. Absent means none; an empty item or a repetition is almost surely
   * a typo, and a wrong pool would only show when the lobby tries to start a node, so it is refused now. */
  private def parsePool(env: Map[String, String]): Either[String, Seq[String]] =
    env.get("TABLE_POOL").fold[Either[String, Seq[String]]](Right(Seq.empty)): raw =>
      val ids = raw.split(",", -1).toSeq.map(_.trim)
      ids.find(!TableProvisioner.isValidId(_)) match
        case Some(bad) => Left(s"Invalid table id '$bad' in TABLE_POOL")
        case None if ids.distinct.size != ids.size => Left("TABLE_POOL has the same table twice")
        case None => Right(ids)

