package server.auth

import utils.TestExports.*

class LoginServiceTest extends AnyFunSuite with BeforeAndAfterEach:

  private val hasher   = Pbkdf2PasswordHasher(iterations = 1_000)
  private val tokens   = SessionTokens("a test secret that is long enough for HMAC".getBytes("UTF-8"))
  private val password = "very secret password"

  var service: LoginService = _

  override def beforeEach(): Unit =
    val users = InMemoryUserRepository()
    RegistrationService(users, hasher).register("Alice", password)
    service = LoginService(users, hasher, tokens)

  test("with the right password the user gets a session whose token tells who they are"):
    val session = service.login("Alice", password).get
    session.username shouldBe "Alice"
    tokens.verify(session.token) shouldBe Some("Alice")

  test("the username can be typed in any case, the session has the registered one"):
    val session = service.login("aLiCe", password).get
    session.username shouldBe "Alice"
    tokens.verify(session.token) shouldBe Some("Alice")

  test("a wrong password or an unknown user gets no session"):
    service.login("Alice", "wrong password") shouldBe None
    service.login("Nobody", password) shouldBe None

  test("a printed session does not show the token"):
    val session = service.login("Alice", password).get
    session.toString should not include session.token
