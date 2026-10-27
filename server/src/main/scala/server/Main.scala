package server

object Main:
  def main(args: Array[String]): Unit =
    NodeConfig.fromEnv(sys.env) match
      case Left(error) =>
        System.err.println(s"Invalid configuration: $error") //fail fast: if the configuration is wrong, the process just fails
        sys.exit(1)
      case Right(config) =>
        val health = HealthServer(config.role, config.port)
        // a table node runs one table: its logic is driven by a single writer thread
        val table    = Option.when(config.role == NodeRole.Table)(SingleWriterHandler(TableHandler()))
        val endpoint = table.map(TableEndpoint(config.wsPort, _))
        sys.addShutdownHook:
          endpoint.foreach(_.stop()) // first stop the network, so no new events arrive...
          table.foreach(_.stop())    // ...then let the table finish the events already queued
          health.stop()
        health.start()
        endpoint.foreach: e =>
          e.start()
          println(s"WebSocket endpoint listening on port ${config.wsPort}")
        println(s"ScalaJack node started: role=${config.role}, port=${config.port}")
        Thread.currentThread().join()
