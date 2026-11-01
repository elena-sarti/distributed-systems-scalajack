package protocol

import utils.TestExports.*
import protocol.LobbyApi.*

class LobbyApiTest extends AnyFunSuite:

  test("a registration request survives a JSON round-trip"):
    val request = RegisterRequest("Alice", "very secret password")
    LobbyApi.decode[RegisterRequest](LobbyApi.encode(request)) shouldBe Right(request)

  test("the answers survive a JSON round-trip"):
    LobbyApi.decode[Registered](LobbyApi.encode(Registered("Alice"))) shouldBe Right(Registered("Alice"))
    LobbyApi.decode[ApiError](LobbyApi.encode(ApiError("Username already taken"))) shouldBe Right(ApiError("Username already taken"))

  test("a registration request is plain JSON with the two fields"):
    LobbyApi.encode(RegisterRequest("Alice", "pw")) shouldBe """{"username":"Alice","password":"pw"}"""

  test("a printed registration request does not show the password"):
    val printed = RegisterRequest("Alice", "very secret password").toString
    printed should include("Alice")
    printed should not include "very secret password"

  test("a malformed body is rejected without throwing and without quoting it"):
    val body = """{"username":"Alice","password":"very secret password""" // not closed
    val result = LobbyApi.decode[RegisterRequest](body)
    result.isLeft shouldBe true
    result.left.toOption.get should not include "very secret password"
