package server.auth

import utils.TestExports.*
import protocol.LobbyApi
import protocol.LobbyApi.{ApiError, RegisterRequest, Registered}
import server.db.{CreateUserError, User, UserRepository}
import server.{HealthServer, NodeRole}
import java.io.{ByteArrayOutputStream, PrintStream}
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

/** `POST /register` served by the lobby's HTTP server, called by a real HTTP client. */
class RegistrationEndpointTest extends AnyFunSuite with BeforeAndAfterEach:

  private val password = "very secret password"
  private val http     = HttpClient.newHttpClient()

  var users: InMemoryUserRepository = _
  var lobby: HealthServer = _

  override def beforeEach(): Unit =
    users = InMemoryUserRepository()
    lobby = startLobby(users)

  override def afterEach(): Unit = lobby.stop()

  private def startLobby(repository: UserRepository): HealthServer =
    val registration = RegistrationService(repository, Pbkdf2PasswordHasher(iterations = 1_000))
    val server = HealthServer(NodeRole.Lobby, 0, Map(LobbyApi.RegisterPath -> RegistrationEndpoint(registration)))
    server.start()
    server

  private def send(request: HttpRequest.Builder, server: HealthServer): HttpResponse[String] =
    http.send(request.build(), HttpResponse.BodyHandlers.ofString())

  private def uri(server: HealthServer, path: String): URI = URI.create(s"http://localhost:${server.boundPort}$path")

  private def post(body: String, server: HealthServer = lobby): HttpResponse[String] =
    send(HttpRequest.newBuilder(uri(server, LobbyApi.RegisterPath)).POST(HttpRequest.BodyPublishers.ofString(body)), server)

  private def register(username: String, password: String, server: HealthServer = lobby): HttpResponse[String] =
    post(LobbyApi.encode(RegisterRequest(username, password)), server)

  private def error(response: HttpResponse[String]): String =
    LobbyApi.decode[ApiError](response.body()).map(_.reason).getOrElse(fail(s"Not an error: ${response.body()}"))

  /** A repository whose database is down. */
  private object BrokenRepository extends UserRepository:
    def create(username: String, passwordHash: String): Either[CreateUserError, User] =
      throw java.sql.SQLException("Connection refused")
    def findByUsername(username: String): Option[User] = throw java.sql.SQLException("Connection refused")

  test("a new user is registered: 201 with the username"):
    val response = register("Alice", password)
    response.statusCode() shouldBe 201
    LobbyApi.decode[Registered](response.body()) shouldBe Right(Registered("Alice"))
    users.findByUsername("Alice").isDefined shouldBe true

  test("a username already taken gets 409 and the reason"):
    register("Alice", password)
    val response = register("ALICE", "another password")
    response.statusCode() shouldBe 409
    error(response) shouldBe RegistrationEndpoint.UsernameTaken

  test("an invalid username or password gets 400 and the reason"):
    val badName = register("Al_ice", password)
    badName.statusCode() shouldBe 400
    error(badName) shouldBe RegistrationEndpoint.InvalidUsername
    val badPassword = register("Alice", "short")
    badPassword.statusCode() shouldBe 400
    error(badPassword) shouldBe RegistrationEndpoint.InvalidPassword
    users.all.isEmpty shouldBe true

  test("a body that is not a registration gets 400"):
    List("", "not json", """{"username":"Alice"}""").foreach: body =>
      val response = post(body)
      response.statusCode() shouldBe 400
      error(response) shouldBe RegistrationEndpoint.Malformed

  test("only POST is accepted"):
    val response = send(HttpRequest.newBuilder(uri(lobby, LobbyApi.RegisterPath)).GET(), lobby)
    response.statusCode() shouldBe 405
    response.headers().firstValue("Allow").orElse("") shouldBe "POST"

  test("a body that is too large is refused before reading it all"):
    val response = register("Alice", "a" * (RegistrationEndpoint.MaxBodyBytes + 1))
    response.statusCode() shouldBe 413
    users.all.isEmpty shouldBe true

  test("if the database is down the client gets 500 and the node keeps answering"):
    val brokenLobby = startLobby(BrokenRepository)
    try
      register("Alice", password, brokenLobby).statusCode() shouldBe 500
      send(HttpRequest.newBuilder(uri(brokenLobby, "/health")).GET(), brokenLobby).statusCode() shouldBe 200
    finally brokenLobby.stop()

  test("the password never appears in the logs, whatever happens to the request"):
    val captured = ByteArrayOutputStream()
    val (out, err) = (System.out, System.err)
    val capture = PrintStream(captured, true, "UTF-8")
    System.setOut(capture)
    System.setErr(capture)
    val brokenLobby = startLobby(BrokenRepository)
    try
      register("Alice", password)                          // registered
      register("Alice", password)                          // duplicate
      register("Al_ice", password)                         // invalid username
      post(s"""{"username":"Alice","password":"$password""") // malformed, with the password inside
      register("Bob", password, brokenLobby)               // database down: an error is logged
    finally
      brokenLobby.stop()
      System.setOut(out)
      System.setErr(err)
    val log = captured.toString("UTF-8")
    log should include("Registration failed") // something was logged...
    log should not include password          // ...but never the password
