package server.db

/** How to reach the PostgreSQL database.
 *
 * @param url      JDBC url, e.g. `jdbc:postgresql://db:5432/scalajack`.
 * @param user     database user.
 * @param password database password (hidden by [[toString]], so it cannot leak in the logs).
 */
final case class DbConfig(url: String, user: String, password: String):
  override def toString: String = s"DbConfig(url=$url, user=$user, password=***)"

object DbConfig:
  /** Pure function (the environment is a parameter), like `NodeConfig.fromEnv`: trivially testable.
   * Reads DB_URL, DB_USER and DB_PASSWORD; all of them are mandatory. */
  def fromEnv(env: Map[String, String]): Either[String, DbConfig] =
    for
      url      <- required("DB_URL", env)
      user     <- required("DB_USER", env)
      password <- required("DB_PASSWORD", env)
    yield DbConfig(url, user, password)

  private def required(name: String, env: Map[String, String]): Either[String, String] =
    env.get(name).filter(_.nonEmpty).toRight(s"$name is not set")
