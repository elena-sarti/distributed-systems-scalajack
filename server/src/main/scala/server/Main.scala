package server

object Main:
  def main(args: Array[String]): Unit =
    NodeConfig.fromEnv(sys.env) match
      case Left(error) =>
        System.err.println(s"Invalid configuration: $error") //fail fast: if the configuration is wrong, the process just fails
        sys.exit(1)
      case Right(config) =>
        val health = HealthServer(config.role, config.port)
        sys.addShutdownHook(health.stop())
        health.start()
        println(s"ScalaJack node started: role=${config.role}, port=${config.port}")
        Thread.currentThread().join()
