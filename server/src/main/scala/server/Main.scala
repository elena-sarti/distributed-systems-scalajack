package server

object Main:
  def main(args: Array[String]): Unit =
    NodeConfig.fromEnv(sys.env) match
      case Left(error) =>
        System.err.println(s"Invalid configuration: $error") //fail fast: if the configuration is wrong, the process just fails
        sys.exit(1)
      case Right(config) =>
        val health = HealthServer(config.role, config.port)
        val endpoint = Option.when(config.role == NodeRole.Table)(TableEndpoint(config.wsPort, NotReadyHandler))
        sys.addShutdownHook:
          endpoint.foreach(_.stop())
          health.stop()
        health.start()
        endpoint.foreach: e =>
          e.start()
          println(s"WebSocket endpoint listening on port ${config.wsPort}")
        println(s"ScalaJack node started: role=${config.role}, port=${config.port}")
        Thread.currentThread().join()
