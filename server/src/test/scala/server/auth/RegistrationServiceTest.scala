package server.auth

import utils.TestExports.*
import server.auth.RegistrationError.*

class RegistrationServiceTest extends AnyFunSuite with BeforeAndAfterEach:

  private val hasher   = Pbkdf2PasswordHasher(iterations = 1_000) 
  private val password = "very secret password"

  var users: InMemoryUserRepository = _
  var registration: RegistrationService = _

  override def beforeEach(): Unit =
    users = InMemoryUserRepository()
    registration = RegistrationService(users, hasher)

  test("a new user is stored with a hash that matches the password"):
    val user = registration.register("Alice", password).toOption.get
    user.username shouldBe "Alice"
    hasher.verify(password, user.passwordHash) shouldBe true
    users.findByUsername("Alice") shouldBe Some(user)

  test("the password itself is never stored"):
    registration.register("Alice", password)
    users.all.map(_.passwordHash).exists(_.contains(password)) shouldBe false

  test("a username already taken is refused, whatever the case, and the first user is kept"):
    val first = registration.register("Alice", password).toOption.get
    registration.register("alice", "another password") shouldBe Left(UsernameTaken)
    users.all shouldBe List(first)

  test("a username that breaks the rules of the table is refused"):
    List("", "Al_ice", " Alice", "Alice ").foreach: username =>
      registration.register(username, password) shouldBe Left(InvalidUsername)
    users.all.isEmpty shouldBe true

  test("a username longer than the database column is refused, one that fits it is accepted"):
    val longest = "A" * RegistrationService.MaxUsernameLength
    registration.register(longest + "A", password) shouldBe Left(InvalidUsername)
    registration.register(longest, password).isRight shouldBe true

  test("a password too short or too long is refused"):
    registration.register("Alice", "a" * (RegistrationService.MinPasswordLength - 1)) shouldBe Left(InvalidPassword)
    registration.register("Alice", "a" * (RegistrationService.MaxPasswordLength + 1)) shouldBe Left(InvalidPassword)
    users.all.isEmpty shouldBe true

  test("passwords of the minimum and of the maximum length are accepted"):
    registration.register("Alice", "a" * RegistrationService.MinPasswordLength).isRight shouldBe true
    registration.register("Bob", "a" * RegistrationService.MaxPasswordLength).isRight shouldBe true
