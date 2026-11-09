package server.funds

import protocol.LobbyApi
import protocol.LobbyApi.{AmountRequest, BalanceRequest, Funds}
import server.auth.{InMemoryUserRepository, SessionTokens, TestLobby}
import server.auth.TestLobby.reason
import utils.TestExports.*

class FundsEndpointsTest extends AnyFunSuite with BeforeAndAfterEach:

  private val tokens = SessionTokens("a test secret that is long enough for HMAC".getBytes("UTF-8"))

  private var users: InMemoryUserRepository = _
  private var lobby: TestLobby              = _

  override def beforeEach(): Unit =
    users = InMemoryUserRepository()
    users.create("Alice", "hash")
    val funds = FundsService(users)
    lobby = TestLobby(Map(
      LobbyApi.BalancePath  -> BalanceEndpoint(tokens, funds),
      LobbyApi.DepositPath  -> FundsEndpoint("Deposit", tokens, funds.deposit),
      LobbyApi.WithdrawPath -> FundsEndpoint("Withdrawal", tokens, funds.withdraw)))

  override def afterEach(): Unit = lobby.stop()

  private val Alice = () => tokens.issue("Alice")

  private def deposit(amount: Double, token: String = Alice()) =
    lobby.post(LobbyApi.DepositPath, LobbyApi.encode(AmountRequest(token, amount)))

  private def withdraw(amount: Double, token: String = Alice()) =
    lobby.post(LobbyApi.WithdrawPath, LobbyApi.encode(AmountRequest(token, amount)))

  private def balance(token: String = Alice()) =
    lobby.post(LobbyApi.BalancePath, LobbyApi.encode(BalanceRequest(token)))

  test("a deposit gets 200 and the new balance, which the balance endpoint then shows"):
    val response = deposit(100.0)
    response.statusCode() shouldBe 200
    LobbyApi.decode[Funds](response.body()) shouldBe Right(Funds(100.0))
    LobbyApi.decode[Funds](balance().body()) shouldBe Right(Funds(100.0))

  test("a withdrawal gets 200 and the new balance"):
    deposit(100.0)
    val response = withdraw(30.0)
    response.statusCode() shouldBe 200
    LobbyApi.decode[Funds](response.body()) shouldBe Right(Funds(70.0))

  test("a new user has a balance of zero"):
    LobbyApi.decode[Funds](balance().body()) shouldBe Right(Funds(0.0))

  test("an invalid amount gets 400 and nothing moves"):
    List(0.0, -1.0, 0.001, FundsService.MaxAmount + 1).foreach: amount =>
      val response = deposit(amount)
      response.statusCode() shouldBe 400
      reason(response) shouldBe FundsEndpoint.InvalidAmount
    users.balanceOf("Alice") shouldBe Some(0.0)

  test("a withdrawal of more than the balance gets 409"):
    deposit(10.0)
    val response = withdraw(10.5)
    response.statusCode() shouldBe 409
    reason(response) shouldBe FundsEndpoint.InsufficientFunds
    users.balanceOf("Alice") shouldBe Some(10.0)

  test("a player sitting at a table gets 409 and is told to leave it first"):
    deposit(100.0)
    users.sitDown("Alice", "table-1")
    List(deposit(10.0), withdraw(10.0)).foreach: response =>
      response.statusCode() shouldBe 409
      reason(response) shouldBe FundsEndpoint.SeatedAtTable
    users.standUp("Alice", "table-1", 100.0)
    deposit(10.0).statusCode() shouldBe 200

  test("a valid token of a user that does not exist gets 404"):
    val ghost = tokens.issue("Nobody")
    List(deposit(10.0, ghost), withdraw(10.0, ghost), balance(ghost)).foreach: response =>
      response.statusCode() shouldBe 404
      reason(response) shouldBe FundsEndpoint.UnknownUser

  test("a missing or forged token gets 401 on every endpoint, and nothing moves"):
    List(deposit(10.0, "not-a-token"), withdraw(10.0, "a.b.c"), balance("not-a-token")).foreach: response =>
      response.statusCode() shouldBe 401
      reason(response) shouldBe server.auth.AuthenticatedEndpoint.InvalidToken
    users.balanceOf("Alice") shouldBe Some(0.0)

  test("a body that is not a request gets 400, and only POST is accepted"):
    List(LobbyApi.BalancePath, LobbyApi.DepositPath, LobbyApi.WithdrawPath).foreach: path =>
      lobby.post(path, "not json").statusCode() shouldBe 400
      lobby.get(path).statusCode() shouldBe 405
