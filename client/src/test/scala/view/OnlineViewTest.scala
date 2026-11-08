package view

import protocol.{DealerView, PlayerView, TableView}
import utils.GameUIExports.*
import utils.TestExports.*
import utils.ModelExports.*
import view.OnlineView.*
import java.nio.charset.Charset
import scala.collection.mutable.ListBuffer

class OnlineViewTest extends AnyFunSuite:

  /** A console that reads the given lines and keeps what is written. */
  private final class ScriptedConsole(lines: String*) extends Console[IO]:
    private val input = Iterator(lines*)
    val output        = ListBuffer[String]()
    override def readLine: IO[String]                                    = IO(input.next())
    def readLineWithCharset(charset: Charset): IO[String]         = readLine
    def print[A](a: A)(using S: Show[A]): IO[Unit]                = IO(output += S.show(a))
    def println[A](a: A)(using S: Show[A]): IO[Unit]              = IO(output += S.show(a))
    def error[A](a: A)(using S: Show[A]): IO[Unit]                = IO.unit
    def errorln[A](a: A)(using S: Show[A]): IO[Unit]              = IO.unit

  private val ace: StandardCard = StandardCard(Suit.Hearts, Value.Ace)
  private val king: StandardCard = StandardCard(Suit.Spades, Value.King)
  private val nine: StandardCard = StandardCard(Suit.Clubs, Value.Nine)

  test("the player logs in or registers, with a username and a password"):
    given ScriptedConsole = ScriptedConsole("x", "r", "", "alice", "", "secret-password")
    askCredentials.unsafeRunSync() shouldBe Credentials(Account.Register, "alice", "secret-password")

  test("a bet must be a multiple of the minimum bet, within the balance; the player can also leave"):
    askBet(100, 5)(using ScriptedConsole("abc", "7", "0", "200", "15")).unsafeRunSync() shouldBe BetChoice.Bet(15)
    askBet(100, 5)(using ScriptedConsole("l")).unsafeRunSync() shouldBe BetChoice.Leave

  test("the insurance and the new game are yes or no questions"):
    given ScriptedConsole = ScriptedConsole("maybe", "y", "N")
    askInsurance().unsafeRunSync() shouldBe true
    askToPlayAgain.unsafeRunSync() shouldBe false

  test("the table shows the dealer with the hidden card, the players with their cards, and whose turn it is"):
    val table = TableView(
      List(PlayerView("Alice", List(ace, king), PlayerState.Blackjack, 90, Some(10)),
        PlayerView("Bob", List(nine), PlayerState.Active, 100, None)),
      DealerView(List(nine), hiddenCards = 1), currentTurn = Some("Bob"), version = 3)
    val text = describeTable(table, me = "Alice")
    text should include("[Alice (you)]")
    text should include("[Bob]  <- turn")
    text should include("BET: 10")
    text should include("BALANCE: 90.0")
    text should include("│A  ♥ │  │K  ♠ │") // the cards next to each other, as in the local game
    text should include("│9  ♣ │  │ ??? │") // the hidden card of the dealer is shown as covered
    text should include("SCORE: 21")

  test("both values of a hand with an ace are shown while it is played, only the best one once it is played"):
    val seven: StandardCard = StandardCard(Suit.Diamonds, Value.Seven)
    def scoreOf(state: PlayerState) =
      describeTable(TableView(List(PlayerView("Alice", List(ace, nine), state, 90, Some(10))),
        DealerView(List(ace, seven), hiddenCards = 0), None, 1), me = "Alice")
    scoreOf(PlayerState.Active) should include("SCORE: 10 / 20   STATE: Active")
    scoreOf(PlayerState.Standing) should include("SCORE: 20   STATE: Standing")
    scoreOf(PlayerState.Standing) should include("[Dealer]\n┌─────┐  ┌─────┐\n│A  ♥ │  │7  ♦ │\n└─────┘  └─────┘\nSCORE: 18")

  test("the results and the end of the game are shown as in the local game"):
    given console: ScriptedConsole = ScriptedConsole()
    (renderResults(List("Alice" -> 110.0)) >> renderGameOver(List("Alice" -> 110.0))).unsafeRunSync()
    console.output.mkString should include("Alice has currently a balance of 110.0 fiches")
    console.output.mkString should include("The game is over!")
