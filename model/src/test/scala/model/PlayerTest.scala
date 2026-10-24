package model

import utils.ModelExports.*
import utils.TestExports.*

class PlayerTest extends AnyFunSuite with BeforeAndAfterEach:

  val startingAmount = 50
  val name = "gigi"
  var player: NormalPlayer = _

  override def beforeEach(): Unit =
    player = NormalPlayer(name, startingAmount)

  test("starting player's state should be always Active"):
    player.state shouldBe PlayerState.Active

  test("starting player's balance should be the exact amount defined by the user"):
    player.balance.totalValue shouldBe startingAmount

  test("player's name should be the one decided by the user"):
    player.name shouldBe name

  test("player's state change should work as expected"):
    player.bust()
    player.state shouldBe PlayerState.Busted
    player.stand()
    player.state shouldBe PlayerState.Standing
    player.winBlackjack()
    player.state shouldBe PlayerState.Blackjack

  test("the deposit method should increase the player's balance by the deposited amount"):
    val depositAmount = 20
    player.deposit(depositAmount)
    player.balance.totalValue shouldBe startingAmount + depositAmount

  test("the withdraw method should work has expected when the bet amount is valid"):
    val bet = 45
    player.withdraw(bet) shouldBe true
    player.balance.totalValue shouldBe startingAmount - bet

  test("the withdraw method should work has expected when the bet amount is not valid"):
    val invalidBet = 55
    player.withdraw(invalidBet) should not be true
    player.balance.totalValue shouldBe startingAmount

  test("adding cards to the player should correctly update their hand"):
    player.cards shouldBe List.empty
    val card1: StandardCard = StandardCard(Suit.Clubs, Value.Two)
    val card2: StandardCard = StandardCard(Suit.Spades, Value.Ace)
    player.addCard(card1)
    player.addCard(card2)
    player.cards shouldBe List(card1, card2)
    player.cards.size shouldBe 2

  test("Player.toString should align cards horizontally and format state correctly"):
    val cards: List[StandardCard] = List(
      StandardCard(Suit.Clubs, Value.Two),
      StandardCard(Suit.Diamonds, Value.Five)
    )
    cards.foreach(player.addCard)
    player.toString.linesIterator.toList match
      case header :: top :: middle :: bottom :: score :: state :: Nil =>
        header shouldBe s"[$name]:"
        top shouldBe    "┌─────┐  ┌─────┐"
        middle shouldBe "│2  ♣ │  │5  ♦ │"
        bottom shouldBe "└─────┘  └─────┘"
        score shouldBe "SCORE: 7"
        state shouldBe "STATE: Active"
      case other =>
        fail(s"The generated layout structure was unexpected. Got:\n${other.mkString("\n")}")

  test("player's initial score with no cards should be 0"):
    player.score shouldBe Score(0, 0)

  test("player's score should be the sum of the cards in hand"):
    val card1: StandardCard = StandardCard(Suit.Clubs, Value.Seven)
    val card2: StandardCard = StandardCard(Suit.Spades, Value.Nine)
    player.addCard(card1)
    player.addCard(card2)
    player.score shouldBe Score(16, 16)

  test("player's score should update dynamically as new cards are added"):
    player.addCard(StandardCard(Suit.Hearts, Value.Ten))
    player.score shouldBe Score(10, 10)

    player.addCard(StandardCard(Suit.Hearts, Value.Five))
    player.score shouldBe Score(15, 15)

    player.addCard(StandardCard(Suit.Hearts, Value.Three))
    player.score shouldBe Score(18, 18)

  test("player's score should reset or be empty when starting a new round"):
    player.addCard(StandardCard(Suit.Hearts, Value.Ten))
    player.addCard(StandardCard(Suit.Hearts, Value.Eight))
    player.score shouldBe Score(18, 18)
    player.prepareForNewHand()
    player.cards shouldBe empty
    player.score shouldBe Score(0, 0)

  test("BotPlayer's random balance and bet should always respect their range and step constraints"):
    val bot = BotPlayer("Bot1")
    bot.balance.totalValue should (be >= 100.0 and be <= 500.0)
    bot.balance.totalValue % 100 shouldBe 0.0
    bot.bet should (be >= 10 and be <= 50)
    bot.bet % 10 shouldBe 0

  test("BotPlayer can be created with a specific values for initialBalance and fixedBet"):
    val bot = BotPlayer("Bot1", 250, 30)
    bot.balance.totalValue shouldBe 250
    bot.bet shouldBe 30

  test("BotPlayer.toString should include the fixed bet after the state line"):
    val bot = BotPlayer("Bot1", 200, 30)
    bot.addCard(StandardCard(Suit.Hearts, Value.Ten))
    bot.toString.linesIterator.toList match
      case header :: top :: middle :: bottom :: score :: state :: bet :: Nil =>
        header shouldBe "[Bot1]:"
        score shouldBe "SCORE: 10"
        state shouldBe "STATE: Active"
        bet shouldBe "BET: 30"
      case other =>
        fail(s"The generated layout structure was unexpected. Got:\n${other.mkString("\n")}")

  test("computeSafeBet should update internal bet and affect toString when bot is broke"):
    val bot = BotPlayer(name = "BrokeBot", initialBalance = 12.0, bet = 50)
    bot.computeSafeBet shouldBe 12
    bot.bet shouldBe 12
    bot.toString should include("BET: 12")
