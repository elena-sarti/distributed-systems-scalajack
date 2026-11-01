package server

/** @param tableId the name of the table run by a table node: it marks the players sitting there in the
 *                database, so it must stay the same when the node starts again. */
final case class NodeConfig(role: NodeRole, port: Int, wsPort: Int = NodeConfig.DefaultWsPort,
                            tableId: String = NodeConfig.DefaultTableId)

object NodeConfig:
  val DefaultPort = 8080
  val DefaultWsPort = 8090 //webSocket port: it's easier to use two different ports for the health HTTP and for the WebSocket
  val DefaultTableId = "table"

  /** Pure function: takes the environment as a parameter, so it is trivially testable. */
  def fromEnv(env: Map[String, String]): Either[String, NodeConfig] =
    for
      rawRole <- env.get("NODE_ROLE").toRight("NODE_ROLE is not set")
      role    <- NodeRole.parse(rawRole)
      port    <- parsePort("NODE_PORT", env, DefaultPort)
      wsPort  <- parsePort("NODE_WS_PORT", env, DefaultWsPort)
      tableId  = env.get("TABLE_ID").map(_.trim).filter(_.nonEmpty).getOrElse(DefaultTableId)
    yield NodeConfig(role, port, wsPort, tableId)

  private def parsePort(name: String, env: Map[String, String], default: Int): Either[String, Int] =
    env.get(name).fold(Right(default)): s =>
      s.toIntOption.filter(p => p >= 1 && p <= 65535).toRight(s"Invalid $name '$s'")