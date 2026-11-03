package server

import utils.TestExports.*
import java.time.Duration

class NodeConfigTest extends AnyFunSuite:

  test("parses the lobby role"):
    NodeConfig.fromEnv(Map("NODE_ROLE" -> "lobby")) shouldBe Right(NodeConfig(NodeRole.Lobby, 8080))

  test("parses the table role, ignoring case and spaces"):
    NodeConfig.fromEnv(Map("NODE_ROLE" -> " Table ")) shouldBe Right(NodeConfig(NodeRole.Table, 8080))

  test("uses the port from NODE_PORT when present"):
    NodeConfig.fromEnv(Map("NODE_ROLE" -> "lobby", "NODE_PORT" -> "9000")) shouldBe
      Right(NodeConfig(NodeRole.Lobby, 9000))

  test("fails when NODE_ROLE is missing"):
    NodeConfig.fromEnv(Map.empty).isLeft shouldBe true

  test("fails on an unknown role"):
    NodeConfig.fromEnv(Map("NODE_ROLE" -> "croupier")).isLeft shouldBe true

  test("fails on a non numeric or out-of-range port"):
    Seq("abc", "0", "70000").foreach: p =>
      NodeConfig.fromEnv(Map("NODE_ROLE" -> "lobby", "NODE_PORT" -> p)).isLeft shouldBe true

  test("uses the default WebSocket port when NODE_WS_PORT is absent"):
    NodeConfig.fromEnv(Map("NODE_ROLE" -> "table")).map(_.wsPort) shouldBe Right(8090)

  test("uses the WebSocket port from NODE_WS_PORT when present"):
    NodeConfig.fromEnv(Map("NODE_ROLE" -> "table", "NODE_WS_PORT" -> "9100")).map(_.wsPort) shouldBe Right(9100)

  test("fails on an invalid WebSocket port"):
    Seq("abc", "0", "70000").foreach: p =>
      NodeConfig.fromEnv(Map("NODE_ROLE" -> "table", "NODE_WS_PORT" -> p)).isLeft shouldBe true

  test("a table node is named by TABLE_ID, with a default when it is absent"):
    NodeConfig.fromEnv(Map("NODE_ROLE" -> "table", "TABLE_ID" -> "table-7")).map(_.tableId) shouldBe Right("table-7")
    NodeConfig.fromEnv(Map("NODE_ROLE" -> "table")).map(_.tableId) shouldBe Right(NodeConfig.DefaultTableId)

  test("a table node announces itself to the lobby only if LOBBY_URL is set"):
    NodeConfig.fromEnv(Map("NODE_ROLE" -> "table")).map(_.lobbyUrl) shouldBe Right(None)
    NodeConfig.fromEnv(Map("NODE_ROLE" -> "table", "LOBBY_URL" -> "http://lobby:8080")).map(_.lobbyUrl) shouldBe
      Right(Some("http://lobby:8080"))

  test("LOBBY_URL must be an http or https address"):
    Seq("lobby:8080", "ftp://lobby", " ").foreach: url =>
      NodeConfig.fromEnv(Map("NODE_ROLE" -> "table", "LOBBY_URL" -> url)).isLeft shouldBe true

  test("the address given to the clients is localhost and the WebSocket port, unless TABLE_HOST and TABLE_PUBLIC_WS_PORT say otherwise"):
    val default = NodeConfig.fromEnv(Map("NODE_ROLE" -> "table", "NODE_WS_PORT" -> "9100")).toOption.get
    (default.publicHost, default.advertisedWsPort) shouldBe ("localhost", 9100)
    val custom = NodeConfig.fromEnv(Map("NODE_ROLE" -> "table", "TABLE_HOST" -> "casino.example", "TABLE_PUBLIC_WS_PORT" -> "9091")).toOption.get
    (custom.publicHost, custom.advertisedWsPort) shouldBe ("casino.example", 9091)

  test("an invalid TABLE_PUBLIC_WS_PORT is refused"):
    NodeConfig.fromEnv(Map("NODE_ROLE" -> "table", "TABLE_PUBLIC_WS_PORT" -> "0")).isLeft shouldBe true

  test("heartbeat period and timeout have defaults, can be set in seconds, and the timeout must be longer than the period"):
    NodeConfig.fromEnv(Map("NODE_ROLE" -> "lobby")).map(_.heartbeat) shouldBe Right(HeartbeatConfig.Default)
    NodeConfig.fromEnv(Map("NODE_ROLE" -> "lobby", "HEARTBEAT_PERIOD_SECONDS" -> "1", "HEARTBEAT_TIMEOUT_SECONDS" -> "3"))
      .map(_.heartbeat) shouldBe Right(HeartbeatConfig(Duration.ofSeconds(1), Duration.ofSeconds(3)))
    Seq(("abc", "3"), ("0", "3"), ("5", "5"), ("5", "2")).foreach: (period, timeout) =>
      NodeConfig.fromEnv(Map("NODE_ROLE" -> "lobby", "HEARTBEAT_PERIOD_SECONDS" -> period, "HEARTBEAT_TIMEOUT_SECONDS" -> timeout))
        .isLeft shouldBe true

  test("the tables the lobby may start are listed in TABLE_POOL, and there are none by default"):
    NodeConfig.fromEnv(Map("NODE_ROLE" -> "lobby")).map(_.tablePool) shouldBe Right(Seq.empty)
    NodeConfig.fromEnv(Map("NODE_ROLE" -> "lobby", "TABLE_POOL" -> "table-1, table-2 ,table-3")).map(_.tablePool) shouldBe
      Right(Seq("table-1", "table-2", "table-3"))

  test("TABLE_POOL must hold valid names, without empty items or repetitions"):
    Seq("table-1,,table-2", "table-1,table-1", "bad/name", "../x", " ").foreach: pool =>
      NodeConfig.fromEnv(Map("NODE_ROLE" -> "lobby", "TABLE_POOL" -> pool)).isLeft shouldBe true

  test("the Docker socket is the standard one unless DOCKER_SOCKET says otherwise"):
    NodeConfig.fromEnv(Map("NODE_ROLE" -> "lobby")).map(_.dockerSocket) shouldBe Right("/var/run/docker.sock")
    NodeConfig.fromEnv(Map("NODE_ROLE" -> "lobby", "DOCKER_SOCKET" -> "/tmp/docker.sock")).map(_.dockerSocket) shouldBe
      Right("/tmp/docker.sock")
