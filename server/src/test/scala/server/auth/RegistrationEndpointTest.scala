package server.auth

import utils.TestExports.*
import protocol.LobbyApi
import protocol.LobbyApi.{RegisterRequest, Registered}
import server.auth.TestLobby.{capturedOutput, reason}
import server.db.UserRepository

class RegistrationEndpointTest extends AnyFunSuite with BeforeAndAfterEach:

  private val password = "very secret password"

  var users: InMemoryUserRepository = _
  var lobby: TestLobby = _

  override def beforeEach(): Unit =
    users = InMemoryUserRepository()
    lobby = lobbyWith(users)

  override def afterEach(): Unit = lobby.stop()

  private def lobbyWith(repository: UserRepository): TestLobby =
    val registration = RegistrationService(repository, Pbkdf2PasswordHasher(iterations = 1_000))
    TestLobby(Map(LobbyApi.RegisterPath -> RegistrationEndpoint(registration)))

  private def register(username: String, password: String, on: TestLobby = lobby) =
    on.post(LobbyApi.RegisterPath, LobbyApi.encode(RegisterRequest(username, password)))

  test("a new user is registered: 201 with the username"):
    val response = register("Alice", password)
    response.statusCode() shouldBe 201
    LobbyApi.decode[Registered](response.body()) shouldBe Right(Registered("Alice"))
    users.findByUsername("Alice").isDefined shouldBe true

  test("a username already taken gets 409 and the reason"):
    register("Alice", password)
    val response = register("ALICE", "another password")
    response.statusCode() shouldBe 409
    reason(response) shouldBe RegistrationEndpoint.UsernameTaken

  test("an invalid username or password gets 400 and the reason"):
    val badName = register("Al_ice", password)
    badName.statusCode() shouldBe 400
    reason(badName) shouldBe RegistrationEndpoint.InvalidUsername
    val badPassword = register("Alice", "short")
    badPassword.statusCode() shouldBe 400
    reason(badPassword) shouldBe RegistrationEndpoint.InvalidPassword
    users.all.isEmpty shouldBe true

  test("a body that is not a registration gets 400"):
    List("", "not json", """{"username":"Alice"}""").foreach: body =>
      val response = lobby.post(LobbyApi.RegisterPath, body)
      response.statusCode() shouldBe 400
      reason(response) shouldBe JsonPostEndpoint.Malformed

  test("only POST is accepted"):
    val response = lobby.get(LobbyApi.RegisterPath)
    response.statusCode() shouldBe 405
    response.headers().firstValue("Allow").orElse("") shouldBe "POST"

  test("a body that is too large is refused"):
    register("Alice", "a" * (JsonPostEndpoint.MaxBodyBytes + 1)).statusCode() shouldBe 413
    users.all.isEmpty shouldBe true

  test("if the database is down the client gets 500 and the node keeps answering"):
    val broken = lobbyWith(BrokenRepository)
    try
      register("Alice", password, broken).statusCode() shouldBe 500
      broken.get("/health").statusCode() shouldBe 200
    finally broken.stop()

  test("the password never appears in the logs, whatever happens to the request"):
    val broken = lobbyWith(BrokenRepository)
    val log = capturedOutput:
      try
        register("Alice", password)                                                 // registered
        register("Alice", password)                                                 // duplicate
        register("Al_ice", password)                                                // invalid username
        lobby.post(LobbyApi.RegisterPath, s"""{"username":"Alice","password":"$password""") // malformed
        register("Bob", password, broken)                                           // database down
      finally broken.stop()
    log should include("Registration failed") // something was logged...
    log should not include password          // ...but never the password
