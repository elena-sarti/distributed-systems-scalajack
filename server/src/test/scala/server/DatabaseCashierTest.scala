package server

import utils.TestExports.*
import server.auth.{BrokenBalances, InMemoryUserRepository, TestLobby}

class DatabaseCashierTest extends AnyFunSuite with BeforeAndAfterEach:

  var balances: InMemoryUserRepository = _

  override def beforeEach(): Unit =
    balances = InMemoryUserRepository()
    balances.create("Alice", "hash")

  test("a player sits down with the saved balance and leaves saving the new one"):
    val cashier = DatabaseCashier(balances, "table-1")
    cashier.sitDown("Alice") shouldBe Right(100.0)
    cashier.standUp("Alice", 125.0)
    balances.balanceOf("Alice") shouldBe Some(125.0)

  test("a player already sitting at another table is refused"):
    DatabaseCashier(balances, "table-1").sitDown("Alice")
    DatabaseCashier(balances, "table-2").sitDown("Alice") shouldBe Left(DatabaseCashier.SeatedElsewhere)

  test("an unknown user is refused"):
    DatabaseCashier(balances, "table-1").sitDown("Nobody") shouldBe Left(DatabaseCashier.UnknownUser)

  test("if the database cannot be reached the player is refused, and the table goes on without errors"):
    val cashier = DatabaseCashier(BrokenBalances, "table-1")
    val log = TestLobby.capturedOutput:
      cashier.sitDown("Alice") shouldBe Left(DatabaseCashier.Unavailable)
      cashier.standUp("Alice", 125.0) // must not throw
    log should include("Cannot save the balance of Alice")
