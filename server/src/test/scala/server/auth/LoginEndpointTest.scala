package server.auth

import utils.TestExports.*
import protocol.LobbyApi
import protocol.LobbyApi.{LoggedIn, LoginRequest}
import server.auth.TestLobby.{capturedOutput, reason}
import server.db.{BalanceRepository, UserRepository}

class LoginEndpointTest extends AnyFunSuite with BeforeAndAfterEach:

  private val hasher   = Pbkdf2PasswordHasher(iterations = 1_000)
  private val tokens   = SessionTokens("a test secret that is long enough for HMAC".getBytes("UTF-8"))
  private val password = "very secret password"

  var lobby: TestLobby = _

  override def beforeEach(): Unit =
    val users = InMemoryUserRepository()
    RegistrationService(users, hasher).register("Alice", password)
    lobby = lobbyWith(users, users)

  override def afterEach(): Unit = lobby.stop()

  private def lobbyWith(users: UserRepository, balances: BalanceRepository): TestLobby =
    TestLobby(Map(LobbyApi.LoginPath -> LoginEndpoint(LoginService(users, balances, hasher, tokens))))

  private def login(username: String, password: String, on: TestLobby = lobby) =
    on.post(LobbyApi.LoginPath, LobbyApi.encode(LoginRequest(username, password)))

  test("the right credentials get 200 and a token that the tables accept"):
    val response = login("Alice", password)
    response.statusCode() shouldBe 200
    val answer = LobbyApi.decode[LoggedIn](response.body()).toOption.get
    answer.username shouldBe "Alice"
    answer.balance shouldBe 0.0 // nobody starts with money
    tokens.verify(answer.token) shouldBe Some("Alice")

  test("a wrong password and an unknown user get the same 401"):
    val wrongPassword = login("Alice", "wrong password")
    val unknownUser   = login("Nobody", password)
    List(wrongPassword, unknownUser).foreach: response =>
      response.statusCode() shouldBe 401
      reason(response) shouldBe LoginEndpoint.WrongCredentials

  test("a body that is not a login gets 400"):
    val response = lobby.post(LobbyApi.LoginPath, "not json")
    response.statusCode() shouldBe 400
    reason(response) shouldBe JsonPostEndpoint.Malformed

  test("if the database is down the client gets 500"):
    val broken = lobbyWith(BrokenRepository, BrokenBalances)
    try login("Alice", password, broken).statusCode() shouldBe 500
    finally broken.stop()

  test("neither the password nor the token appear in the logs"):
    val broken = lobbyWith(BrokenRepository, BrokenBalances)
    var token = ""
    val log = capturedOutput:
      try
        token = LobbyApi.decode[LoggedIn](login("Alice", password).body()).toOption.get.token
        login("Alice", "wrong password")
        login("Alice", password, broken)
      finally broken.stop()
    log should include("Login failed")
    log should not include password
    log should not include token
