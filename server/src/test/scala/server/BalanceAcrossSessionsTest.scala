package server

import utils.TestExports.*
import utils.ModelExports.{Deck, Game, StandardCard, Suit, Value}
import protocol.ClientMessage.*
import protocol.ServerMessage
import protocol.ServerMessage.*
import server.auth.{InMemoryUserRepository, LoginService, Pbkdf2PasswordHasher, RegistrationService, SessionTokens}
import java.util.UUID
import scala.collection.mutable.ListBuffer

/** The whole path of a balance: login, a hand won at a table, the exit, the next login and the next table. */
class BalanceAcrossSessionsTest extends AnyFunSuite with BeforeAndAfterEach:

  private final class FakeConnection extends Connection:
    val id    = UUID.randomUUID().toString
    val inbox = ListBuffer[ServerMessage]()
    def send(message: ServerMessage): Unit = inbox += message

  private val hasher = Pbkdf2PasswordHasher(iterations = 1_000)
  private val tokens = SessionTokens("a test secret that is long enough for HMAC".getBytes("UTF-8"))

  private var users: InMemoryUserRepository = _
  private var login: LoginService = _

  override def beforeEach(): Unit = // a fresh database for every test
    users = InMemoryUserRepository()
    login = LoginService(users, users, hasher, tokens)
    RegistrationService(users, hasher).register("Alice", "very secret password")
    users.deposit("Alice", 100.0) // a new user has nothing: the first thing to do is a deposit

  /** A table whose deck gives Alice 20 against the dealer's 18. */
  private def table(id: String): TableHandler =
    val cards = List(Value.Ten, Value.Nine, Value.Queen, Value.Seven, Value.Two).map(StandardCard(Suit.Hearts, _))
    TableHandler(cashier = DatabaseCashier(users, id), newGame = () => Game(List.empty, Deck.testDeck(cards*)))

  private def sitDown(at: TableHandler): FakeConnection =
    val alice = FakeConnection()
    at.onMessage(alice, JoinTable("Alice"))
    alice

  test("the balance saved when leaving a table is the one found at the next login, and at the next table"):
    login.login("Alice", "very secret password").get.balance shouldBe 100.0
    val first = table("table-1")
    val alice = sitDown(first)
    first.onMessage(alice, PlaceBet(10))
    first.onMessage(alice, Stand) // 20 against 18: +10
    first.onMessage(alice, LeaveTable)
    login.login("Alice", "very secret password").get.balance shouldBe 110.0
    sitDown(table("table-2")).inbox.head shouldBe JoinAccepted("Alice", 110.0)

  test("while sitting at a table a player cannot sit at another one"):
    val first = table("table-1")
    val alice = sitDown(first)
    sitDown(table("table-2")).inbox.toList shouldBe List(JoinRejected(DatabaseCashier.SeatedElsewhere))
    first.onMessage(alice, LeaveTable)
    sitDown(table("table-2")).inbox.head shouldBe JoinAccepted("Alice", 100.0)
