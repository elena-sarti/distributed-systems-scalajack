package server.funds

import server.auth.InMemoryUserRepository
import utils.TestExports.*

class FundsServiceTest extends AnyFunSuite with BeforeAndAfterEach:

  private var users: InMemoryUserRepository = _
  private var funds: FundsService           = _

  override def beforeEach(): Unit =
    users = InMemoryUserRepository()
    users.create("Alice", "hash")
    funds = FundsService(users)

  test("a new user has no money"):
    funds.balance("Alice") shouldBe Right(0.0)

  test("the balance of an unknown user is refused"):
    funds.balance("Nobody") shouldBe Left(FundsRefusal.UnknownUser)

  test("a deposit gives the new balance, and a withdrawal too"):
    funds.deposit("Alice", 100.0) shouldBe Right(100.0)
    funds.deposit("Alice", 12.5) shouldBe Right(112.5)
    funds.withdraw("Alice", 50.0) shouldBe Right(62.5)
    funds.balance("Alice") shouldBe Right(62.5)

  test("an amount must be positive, finite, with at most two decimals and not above the maximum"):
    val invalid = List(0.0, -5.0, Double.NaN, Double.PositiveInfinity, 10.001, FundsService.MaxAmount + 1)
    invalid.foreach: amount =>
      withClue(s"amount $amount: "):
        FundsService.isAmountValid(amount) shouldBe false
        funds.deposit("Alice", amount) shouldBe Left(FundsRefusal.InvalidAmount)
        funds.withdraw("Alice", amount) shouldBe Left(FundsRefusal.InvalidAmount)
    funds.balance("Alice") shouldBe Right(0.0) // nothing moved
    List(0.01, 5.0, 99.99, FundsService.MaxAmount).foreach(amount => FundsService.isAmountValid(amount) shouldBe true)

  test("a withdrawal of more than the balance is refused and the balance stays"):
    funds.deposit("Alice", 20.0)
    funds.withdraw("Alice", 20.5) shouldBe Left(FundsRefusal.InsufficientFunds)
    funds.balance("Alice") shouldBe Right(20.0)

  test("an unknown user cannot deposit nor withdraw"):
    funds.deposit("Nobody", 10.0) shouldBe Left(FundsRefusal.UnknownUser)
    funds.withdraw("Nobody", 10.0) shouldBe Left(FundsRefusal.UnknownUser)

  test("a player sitting at a table cannot move their money"):
    funds.deposit("Alice", 100.0)
    users.sitDown("Alice", "table-1")
    funds.deposit("Alice", 10.0) shouldBe Left(FundsRefusal.SeatedAtTable)
    funds.withdraw("Alice", 10.0) shouldBe Left(FundsRefusal.SeatedAtTable)
    users.standUp("Alice", "table-1", 100.0)
    funds.deposit("Alice", 10.0) shouldBe Right(110.0) // free again

  test("only a player with at least the minimum bet can play"):
    funds.canPlay("Alice") shouldBe false                    // no money
    funds.deposit("Alice", 4.5)
    funds.canPlay("Alice") shouldBe false                    // not enough for the minimum bet (5)
    funds.deposit("Alice", 0.5)
    funds.canPlay("Alice") shouldBe true                     // exactly the minimum bet
    funds.withdraw("Alice", 1.0)
    funds.canPlay("Alice") shouldBe false                    // the withdrawal took the player under it
    funds.canPlay("Nobody") shouldBe false
