package server

import utils.TestExports.*

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