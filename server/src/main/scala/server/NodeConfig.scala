package server

final case class NodeConfig(role: NodeRole, port: Int)

object NodeConfig:
  val DefaultPort = 8080

  /** Pure function: takes the environment as a parameter, so it is trivially testable. */
  def fromEnv(env: Map[String, String]): Either[String, NodeConfig] =
    for
      rawRole <- env.get("NODE_ROLE").toRight("NODE_ROLE is not set")
      role    <- NodeRole.parse(rawRole)
      port    <- parsePort(env.get("NODE_PORT"))
    yield NodeConfig(role, port)

  private def parsePort(raw: Option[String]): Either[String, Int] =
    raw.fold(Right(DefaultPort)): s =>
      s.toIntOption.filter(p => p >= 1 && p <= 65535).toRight(s"Invalid port '$s'")