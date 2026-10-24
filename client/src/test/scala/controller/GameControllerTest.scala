package controller

import utils.GameUIExports.*
import utils.TestExports.*
import utils.ModelExports.*
import controller.GameController.*

import java.nio.charset.Charset

trait ControllerTestConstants:
  // Balances
  val DefaultP1Balance = 50.0
  val DefaultP2Balance = 100.0
  val DefaultBotBalance = 150.0

  // Bets & Amounts
  val StandardBetAmount = 20
  val LowTestBet = 10
  val HighTestBet = 30
  val InsufficientBetAmount = 200

  // Multipliers & Thresholds
  val DoubleDownMultiplier = 2
  val BlackjackPayoutMultiplier = 2.5
  val InsuranceMultiplier = 0.5
  val StandardWinMultiplier = 2
  val DealerStandThreshold = 17

class GameControllerTest extends AnyFunSuite with BeforeAndAfterEach with ControllerTestConstants:

  var player1: NormalPlayer = _
  var player2: NormalPlayer = _
  var game: Game = _
  var outputMessages: List[String] = _

  override def beforeEach(): Unit =
    player1 = NormalPlayer("P1", DefaultP1Balance)
    player2 = NormalPlayer("P2", DefaultP2Balance)
    game = Game(List(player1, player2))
    outputMessages = List.empty[String]

  def mockConsoleWith(readLineBehavior: () => String): Console[IO] = new Console[IO]:
    override def readLine: IO[String] = IO(readLineBehavior())
    override def readLineWithCharset(charset: Charset): IO[String] = readLine
    override def print[A](a: A)(using S: Show[A]): IO[Unit] = IO.unit
    override def println[A](a: A)(using S: Show[A]): IO[Unit] = IO:
      outputMessages = outputMessages :+ S.show(a)
    override def error[A](a: A)(using S: Show[A]): IO[Unit] = IO.unit
    override def errorln[A](a: A)(using S: Show[A]): IO[Unit] = IO.unit

  test("Players count should match the requested number."):
    val requestedCount = 2
    val simulatedInputs = Iterator("P1, P2", DefaultP1Balance.toInt.toString, DefaultP2Balance.toInt.toString)
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    val players = getPlayers(requestedCount).unsafeRunSync()
    players.length shouldBe requestedCount

  test("Players should have the correct names."):
    val simulatedInputs = Iterator("P1, P2", DefaultP1Balance.toInt.toString, DefaultP2Balance.toInt.toString)
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    val players = getPlayers(2).unsafeRunSync()
    players.map(_.name) shouldBe List("P1", "P2")

  test("Players should have the correct starting balance."):
    val simulatedInputs = Iterator("P1, P2", DefaultP1Balance.toInt.toString, DefaultP2Balance.toInt.toString)
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    val players = getPlayers(2).unsafeRunSync()
    players.map(_.balance.totalValue) shouldBe List(DefaultP1Balance, DefaultP2Balance)

  test("The game size should equal max players number."):
    val simulatedInputs = Iterator(
      game.players.size.toString,
      s"${player1.name}, ${player2.name}",
      player1.balance.totalValue.toString,
      player2.balance.totalValue.toString
    )
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    val actualGame: Game = initializeGame.unsafeRunSync()
    actualGame.players.size shouldBe Game.MaxPlayersNum

  test("Human players should sit in the first seats."):
    val expectedHumanNames = List(player1.name, player2.name)
    val simulatedInputs = Iterator(
      game.players.size.toString,
      expectedHumanNames.mkString(", "),
      player1.balance.totalValue.toString,
      player2.balance.totalValue.toString
    )
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    val actualGame: Game = initializeGame.unsafeRunSync()
    actualGame.players.take(expectedHumanNames.size).map(_.name) shouldBe expectedHumanNames

  test("A game should have a non null dealer."):
    val simulatedInputs = Iterator(
      game.players.size.toString,
      s"${player1.name}, ${player2.name}",
      player1.balance.totalValue.toString,
      player2.balance.totalValue.toString
    )
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    val actualGame: Game = initializeGame.unsafeRunSync()
    actualGame.dealer should not be null

  test("Human players should keep their initial balance"):
    val expectedBalances = List(DefaultP1Balance, DefaultP2Balance)
    val simulatedInputs = Iterator(
      game.players.size.toString,
      s"${player1.name}, ${player2.name}",
      player1.balance.totalValue.toString,
      player2.balance.totalValue.toString
    )
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    val actualGame: Game = initializeGame.unsafeRunSync()
    actualGame.players.take(expectedBalances.size).map(_.balance.totalValue) shouldBe expectedBalances

  test("Remaining slots should be filled with bot players"):
    val humanCount = 2
    val simulatedInputs = Iterator(
      game.players.size.toString,
      s"${player1.name}, ${player2.name}",
      player1.balance.totalValue.toString,
      player2.balance.totalValue.toString
    )
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    val actualGame: Game = initializeGame.unsafeRunSync()
    actualGame.players.drop(humanCount).foreach(_.isInstanceOf[BotPlayer] shouldBe true)

  test("The table size should equal the maximum number of players"):
    val simulatedInputs = Iterator(
      game.players.size.toString,
      s"${player1.name}, ${player2.name}",
      player1.balance.totalValue.toString,
      player2.balance.totalValue.toString
    )
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    val actualGame: Game = initializeGame.unsafeRunSync()
    actualGame.players.size shouldBe Game.MaxPlayersNum

  test("Current bets should store the entered amounts."):
    val simulatedInputs = Iterator("invalid_bet", StandardBetAmount.toString, StandardBetAmount.toString)
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    getBets(game).unsafeRunSync()
    game.currentBets.map(_.amount) shouldBe List(StandardBetAmount, StandardBetAmount)

  test("A player balance should be decreased by bet amount"):
    val simulatedInputs = Iterator("invalid_bet", StandardBetAmount.toString, StandardBetAmount.toString)
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    getBets(game).unsafeRunSync()
    player1.balance.totalValue shouldBe (DefaultP1Balance - StandardBetAmount)
    player2.balance.totalValue shouldBe (DefaultP2Balance - StandardBetAmount)

  test("Bot bets should be correctly recorded."):
    val healthyBot = BotPlayer(name = "Bot1", initialBalance = DefaultBotBalance, bet = StandardBetAmount)
    val brokeBot = BotPlayer(name = "Bot2", initialBalance = DefaultBotBalance, bet = StandardBetAmount)
    val botList = List(healthyBot, brokeBot)
    val game = Game(botList)
    implicit val mockConsole: Console[IO] = mockConsoleWith(() => "")
    GameController.getBets(game).unsafeRunSync()
    game.currentBets.map(_.amount) shouldBe List(StandardBetAmount, StandardBetAmount)

  test("A whealty bot balance should be decreased by bet amount."):
    val healthyBot = BotPlayer(name = "Bot1", initialBalance = DefaultBotBalance, bet = StandardBetAmount)
    val brokeBot = BotPlayer(name = "Bot2", initialBalance = DefaultBotBalance, bet = StandardBetAmount)
    val botList = List(healthyBot, brokeBot)
    val game = Game(botList)
    implicit val mockConsole: Console[IO] = mockConsoleWith(() => "")
    GameController.getBets(game).unsafeRunSync()
    healthyBot.balance.totalValue shouldBe (DefaultBotBalance - StandardBetAmount)

  test("The amount of the bet of a broke bot should be fixed to its current balance."):
    val healthyBot = BotPlayer(name = "Bot1", initialBalance = DefaultBotBalance, bet = StandardBetAmount)
    val brokeBot = BotPlayer(name = "Bot2", initialBalance = DefaultBotBalance, bet = InsufficientBetAmount)
    val botList = List(healthyBot, brokeBot)
    val game = Game(botList)
    implicit val mockConsole: Console[IO] = mockConsoleWith(() => "")
    GameController.getBets(game).unsafeRunSync()
    brokeBot.bet shouldBe DefaultBotBalance

  test("The bets of a hand should match the user inputs."):
    val testDeck = Deck.testDeck(
      StandardCard(Suit.Hearts, Value.Ace),
      StandardCard(Suit.Hearts, Value.Six),
      StandardCard(Suit.Hearts, Value.Four),
      StandardCard(Suit.Hearts, Value.Seven),
      StandardCard(Suit.Clubs, Value.Four),
      StandardCard(Suit.Diamonds, Value.Six)
    )
    game = Game(List(player1, player2), deck = testDeck)
    val simulatedInputs = Iterator(StandardBetAmount.toString, StandardBetAmount.toString)
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    initializeHand(game).unsafeRunSync()
    game.currentBets.map(_.amount) shouldBe List(StandardBetAmount, StandardBetAmount)

  test("Every participant should receive two cards."):
    val expectedCardsCount = 2
    val testDeck = Deck.testDeck(StandardCard(Suit.Hearts, Value.Ace),
      StandardCard(Suit.Hearts, Value.Six),
      StandardCard(Suit.Hearts, Value.Four),
      StandardCard(Suit.Hearts, Value.Seven),
      StandardCard(Suit.Clubs, Value.Four),
      StandardCard(Suit.Diamonds, Value.Six)
    )
    game = Game(List(player1, player2), deck = testDeck)
    val participants = game.players :+ game.dealer
    val simulatedInputs = Iterator(HighTestBet.toString, StandardBetAmount.toString)
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    initializeHand(game).unsafeRunSync()
    participants.foreach(_.cards.size shouldBe expectedCardsCount)

  test("Insurance prompt should be printed when dealer has an ace."):
    game.dealer.addCard(StandardCard(Suit.Hearts, Value.Ace))
    val simulatedInputs = Iterator(LowTestBet.toString, StandardBetAmount.toString, player1.name)
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    initializeHand(game).unsafeRunSync()
    outputMessages.exists(_.contains("Insurance")) shouldBe true

  test("A player's insurance bet should be recorded."):
    game.dealer.addCard(StandardCard(Suit.Hearts, Value.Ace))
    val simulatedInputs = Iterator(StandardBetAmount.toString, StandardBetAmount.toString, player1.name)
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    initializeHand(game).unsafeRunSync()
    val insuranceCost = (StandardBetAmount * InsuranceMultiplier).toInt
    val betP1 = Bet(player1, StandardBetAmount + insuranceCost)
    game.currentBets should contain(betP1)

  test("A player standard bet should remain unchanged if no insurance is made."):
    game.dealer.addCard(StandardCard(Suit.Hearts, Value.Ace))
    val simulatedInputs = Iterator(StandardBetAmount.toString, StandardBetAmount.toString, player1.name)
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    initializeHand(game).unsafeRunSync()
    val betP2 = Bet(player2, StandardBetAmount)
    game.currentBets should contain(betP2)

  test("Insurance request should be skipped if dealer does not have an ace."):
    game.dealer.addCard(StandardCard(Suit.Hearts, Value.King))
    val simulatedInputs = Iterator(LowTestBet.toString, StandardBetAmount.toString, player1.name)
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    initializeHand(game).unsafeRunSync()
    outputMessages.exists(_.contains("Insurance")) shouldBe false

  test("Blackjack payout should use the correct bonus multiplier."):
    game.currentBets = List(Bet(player1, StandardBetAmount), Bet(player2, StandardBetAmount))
    game.dealer.addCard(StandardCard(Suit.Hearts, Value.Six))
    player1.addCard(StandardCard(Suit.Hearts, Value.Ace))
    player1.addCard(StandardCard(Suit.Spades, Value.King))
    given mockConsole: Console[IO] = mockConsoleWith(() => "")
    handleBlackjacksWinners(game).unsafeRunSync()
    player1.balance.totalValue shouldBe DefaultP1Balance + (BlackjackPayoutMultiplier * StandardBetAmount)

  test("The name of a blackjack winner should be printed."):
    game.currentBets = List(Bet(player1, StandardBetAmount), Bet(player2, StandardBetAmount))
    game.dealer.addCard(StandardCard(Suit.Hearts, Value.Six))
    player1.addCard(StandardCard(Suit.Hearts, Value.Ace))
    player1.addCard(StandardCard(Suit.Spades, Value.King))
    given mockConsole: Console[IO] = mockConsoleWith(() => "")
    handleBlackjacksWinners(game).unsafeRunSync()
    outputMessages.exists(_.contains(player1.name)) shouldBe true

  test("The cut card alert should be printed when drawn."):
    val craftedDeck = Deck.testDeck(
      CutCard,
      StandardCard(Suit.Hearts, Value.Six),
      StandardCard(Suit.Hearts, Value.Four)
    )
    val game = Game(List(player1), craftedDeck)
    given mockConsole: Console[IO] = mockConsoleWith(() => "")
    val result = handlePlayerAction(game, player1, PlayerAction.DrawCard).unsafeRunSync()
    val hasCutCardMessage = outputMessages.exists(_.contains("CUT CARD HAS BEEN EXTRACTED!"))
    hasCutCardMessage shouldBe true

  test("A bot should finish its turn automatically."):
    val bot = BotPlayer(name = "Bot", initialBalance = DefaultBotBalance, bet = StandardBetAmount)
    bot.addCard(StandardCard(Suit.Hearts, Value.Ten))
    val game = Game(List(bot))
    implicit val mockConsole: Console[IO] = mockConsoleWith(() => "")
    GameController.handlePlayersTurn(game).unsafeRunSync()
    bot.hasFinishedTurn shouldBe true

  test("A bot should draw cards during its turn."):
    val bot = BotPlayer(name = "Bot", initialBalance = DefaultBotBalance, bet = StandardBetAmount)
    bot.addCard(StandardCard(Suit.Hearts, Value.Ten))
    val game = Game(List(bot))
    implicit val mockConsole: Console[IO] = mockConsoleWith(() => "")
    GameController.handlePlayersTurn(game).unsafeRunSync()
    bot.cards.size should be > 1

  test("The name of a bot should be printed during its turn"):
    val bot = BotPlayer(name = "Bot", initialBalance = DefaultBotBalance, bet = StandardBetAmount)
    bot.addCard(StandardCard(Suit.Hearts, Value.Ten))
    val game = Game(List(bot))
    implicit val mockConsole: Console[IO] = mockConsoleWith(() => "")
    GameController.handlePlayersTurn(game).unsafeRunSync()
    outputMessages.exists(_.contains(bot.name)) shouldBe true

  test("A player hand size should increase after drawing."):
    val expectedHandSize = 1
    val simulatedInputs = Iterator("D", "S", "S")
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    handlePlayersTurn(game).unsafeRunSync()
    player1.cards.size shouldBe expectedHandSize

  test("The deck size should decrease after drawing a card."):
    val initialDeckSize = 209
    val simulatedInputs = Iterator("D", "S", "S")
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    handlePlayersTurn(game).unsafeRunSync()
    game.deck.size() shouldBe initialDeckSize - 1

  test("A player state should become standing after stand choice"):
    val simulatedInputs = Iterator("D", "S", "S")
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    handlePlayersTurn(game).unsafeRunSync()
    player1.state shouldBe PlayerState.Standing
    player2.state shouldBe PlayerState.Standing

  test("A player with natural blackjack should not draw cards."):
    val emptyHandSize = 0
    player1.winBlackjack()
    val simulatedInputs = Iterator("S")
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    handlePlayersTurn(game).unsafeRunSync()
    player1.cards.size shouldBe emptyHandSize

  test("A player with natural blackjack should keep blackjack state."):
    player1.winBlackjack()
    val simulatedInputs = Iterator("S")
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    handlePlayersTurn(game).unsafeRunSync()
    player1.state shouldBe PlayerState.Blackjack

  test("Subsequent players should still be able to stand."):
    player1.winBlackjack()
    val simulatedInputs = Iterator("S")
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    handlePlayersTurn(game).unsafeRunSync()
    player2.state shouldBe PlayerState.Standing

  test("A player score should be exactly 21 when reaching 21."):
    val winningScore = 21
    val craftedDeck = Deck.testDeck(
      StandardCard(Suit.Hearts, Value.Ace),
      StandardCard(Suit.Hearts, Value.Six),
      StandardCard(Suit.Hearts, Value.Four)
    )
    val testGame = Game(List(player1), craftedDeck)
    val simulatedInputs = Iterator("D", "D", "D")
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    handlePlayersTurn(testGame).unsafeRunSync()
    player1.score.playableValue shouldBe winningScore

  test("A player state should become standing after automatic stop at 21."):
    val craftedDeck = Deck.testDeck(
      StandardCard(Suit.Hearts, Value.Ace),
      StandardCard(Suit.Hearts, Value.Six),
      StandardCard(Suit.Hearts, Value.Four)
    )
    val testGame = Game(List(player1), craftedDeck)
    val simulatedInputs = Iterator("D", "D", "D")
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    handlePlayersTurn(testGame).unsafeRunSync()
    player1.state shouldBe PlayerState.Standing

  test("A test deck should be empty after drawing all cards."):
    val emptyDeckSize = 0
    val craftedDeck = Deck.testDeck(
      StandardCard(Suit.Hearts, Value.Ace),
      StandardCard(Suit.Hearts, Value.Six),
      StandardCard(Suit.Hearts, Value.Four))
    val testGame = Game(List(player1), craftedDeck)
    val simulatedInputs = Iterator("D", "D", "D")
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    handlePlayersTurn(testGame).unsafeRunSync()
    testGame.deck.size() shouldBe emptyDeckSize

  test("The players list size should increase after a split"):
    val expectedPlayersCountAfterSplit = 3
    game.currentBets = List(Bet(player1, StandardBetAmount))
    player1.addCard(StandardCard(Suit.Spades, Value.Six))
    player1.addCard(StandardCard(Suit.Hearts, Value.Six))
    val simulatedInputs = Iterator("P", "S", "S", "S")
    given Console[IO] = mockConsoleWith(() => simulatedInputs.next)
    handlePlayersTurn(game).unsafeRunSync()
    game.players.size shouldBe expectedPlayersCountAfterSplit

  test("A split player should have a specific modified name."):
    val splitPlayerIndex = 1
    game.currentBets = List(Bet(player1, StandardBetAmount))
    player1.addCard(StandardCard(Suit.Spades, Value.Six))
    player1.addCard(StandardCard(Suit.Hearts, Value.Six))
    val simulatedInputs = Iterator("P", "S", "S", "S")
    given Console[IO] = mockConsoleWith(() => simulatedInputs.next)
    handlePlayersTurn(game).unsafeRunSync()
    game.players(splitPlayerIndex).name shouldBe "P1_split1"

  test("The original player balance should decrease after split cost."):
    val expectedBalanceAfterSplit = 0.0
    game.currentBets = List(Bet(player1, StandardBetAmount))
    player1.addCard(StandardCard(Suit.Spades, Value.Six))
    player1.addCard(StandardCard(Suit.Hearts, Value.Six))
    val simulatedInputs = Iterator("P", "S", "S", "S")
    given Console[IO] = mockConsoleWith(() => simulatedInputs.next)
    handlePlayersTurn(game).unsafeRunSync()
    player1.balance.totalValue shouldBe expectedBalanceAfterSplit

  test("Both hands should get blackjack after split if lucky."):
    val deck = Deck.testDeck(
      StandardCard(Suit.Spades, Value.Queen),
      StandardCard(Suit.Hearts, Value.Six),
      StandardCard(Suit.Spades, Value.Queen),
      StandardCard(Suit.Hearts, Value.Six),
      StandardCard(Suit.Spades, Value.Ace),
      StandardCard(Suit.Spades, Value.Ace))
    val game = Game(List(player1), deck)
    game.currentBets = List(Bet(player1, StandardBetAmount))
    val simulatedInputs = Iterator("P")
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    game.distributeCards()
    handlePlayersTurn(game).unsafeRunSync()
    game.players.map(_.state) shouldBe List(PlayerState.Blackjack, PlayerState.Blackjack)

  test("A player should receive a third card on double down"):
    val expectedCardsCount = 3
    val testGame = Game(List(player1), Deck.testDeck(StandardCard(Suit.Hearts, Value.Six)))
    testGame.currentBets = List(Bet(player1, StandardBetAmount))
    player1.addCard(StandardCard(Suit.Hearts, Value.Six))
    player1.addCard(StandardCard(Suit.Spades, Value.Seven))
    val simulatedInputs = Iterator("O")
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    handlePlayersTurn(testGame).unsafeRunSync()
    player1.cards.size shouldBe expectedCardsCount

  test("A bet amount should double on double down."):
    val testGame = Game(List(player1), Deck.testDeck(StandardCard(Suit.Hearts, Value.Six)))
    testGame.currentBets = List(Bet(player1, StandardBetAmount))
    player1.addCard(StandardCard(Suit.Hearts, Value.Six))
    player1.addCard(StandardCard(Suit.Spades, Value.Seven))
    val simulatedInputs = Iterator("O")
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    handlePlayersTurn(testGame).unsafeRunSync()
    testGame.currentBets shouldBe List(Bet(player1, StandardBetAmount * DoubleDownMultiplier))

  test("A player state should become standing after double down."):
    val testGame = Game(List(player1), Deck.testDeck(StandardCard(Suit.Hearts, Value.Six)))
    testGame.currentBets = List(Bet(player1, StandardBetAmount))
    player1.addCard(StandardCard(Suit.Hearts, Value.Six))
    player1.addCard(StandardCard(Suit.Spades, Value.Seven))
    val simulatedInputs = Iterator("O")
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    handlePlayersTurn(testGame).unsafeRunSync()
    player1.state shouldBe PlayerState.Standing

  test("A player state should become busted if double down exceeds twenty one."):
    val game = Game(List(player1), Deck.testDeck(StandardCard(Suit.Hearts, Value.King)))
    game.currentBets = List(Bet(player1, StandardBetAmount))
    player1.addCard(StandardCard(Suit.Hearts, Value.Ten))
    player1.addCard(StandardCard(Suit.Spades, Value.Five))
    val simulatedInputs = Iterator("O")
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    handlePlayersTurn(game).unsafeRunSync()
    player1.state shouldBe PlayerState.Busted

  test("A bet should not double if hit is chosen before double down."):
    val game = Game(List(player1), Deck.testDeck(StandardCard(Suit.Hearts, Value.Two), StandardCard(Suit.Hearts, Value.Three)))
    game.currentBets = List(Bet(player1, StandardBetAmount))
    player1.addCard(StandardCard(Suit.Hearts, Value.Six))
    player1.addCard(StandardCard(Suit.Spades, Value.Seven))
    val simulatedInputs = Iterator("D", "O", "S")
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    handlePlayersTurn(game).unsafeRunSync()
    game.currentBets shouldBe List(Bet(player1, StandardBetAmount))

  test("A player state should become standing after invalid double down path."):
    val game = Game(List(player1), Deck.testDeck(StandardCard(Suit.Hearts, Value.Two), StandardCard(Suit.Hearts, Value.Three)))
    game.currentBets = List(Bet(player1, StandardBetAmount))
    player1.addCard(StandardCard(Suit.Hearts, Value.Six))
    player1.addCard(StandardCard(Suit.Spades, Value.Seven))
    val simulatedInputs = Iterator("D", "O", "S")
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    handlePlayersTurn(game).unsafeRunSync()
    player1.state shouldBe PlayerState.Standing

  test("The dealer should draw cards during their turn."):
    val initialDealerCards = 2
    game.dealer.addCard(StandardCard(Suit.Hearts, Value.Six))
    game.dealer.addCard(StandardCard(Suit.Spades, Value.Five))
    given mockConsole: Console[IO] = mockConsoleWith(() => "")
    handleDealerTurn(game).unsafeRunSync()
    game.dealer.cards.size should be > initialDealerCards

  test("The dealer final score should be at least seventeen."):
    game.dealer.addCard(StandardCard(Suit.Hearts, Value.Six))
    game.dealer.addCard(StandardCard(Suit.Spades, Value.Five))
    given mockConsole: Console[IO] = mockConsoleWith(() => "")
    handleDealerTurn(game).unsafeRunSync()
    game.dealer.score.maxValue should be >= DealerStandThreshold

  test("An insured player should be notified on dealer blackjack."):
    game.dealer.addCard(StandardCard(Suit.Hearts, Value.Ace))
    game.dealer.addCard(StandardCard(Suit.Spades, Value.King, isFaceUp = false))
    player1.hasInsurance = true
    game.currentBets = List(Bet(player1, StandardBetAmount), Bet(player2, StandardBetAmount))
    given mockConsole: Console[IO] = mockConsoleWith(() => "")
    handleDealerTurn(game).unsafeRunSync()
    outputMessages.exists(_.contains(player1.name)) shouldBe true

  test("A player balance should increase after dealer bust."):
    game.currentBets = List(Bet(player1, StandardBetAmount))
    game.dealer.addCard(StandardCard(Suit.Hearts, Value.Ten))
    game.dealer.addCard(StandardCard(Suit.Spades, Value.Ten))
    game.dealer.addCard(StandardCard(Suit.Clubs, Value.Five))
    player1.addCard(StandardCard(Suit.Hearts, Value.Ten))
    player1.addCard(StandardCard(Suit.Clubs, Value.Eight))
    val startingBalance = player1.balance.totalValue
    given mockConsole: Console[IO] = mockConsoleWith(() => "")
    handleHandWinners(game).unsafeRunSync()
    player1.balance.totalValue shouldBe startingBalance + (StandardBetAmount * StandardWinMultiplier)

  test("The dealer busted message should be printed."):
    game.currentBets = List(Bet(player1, StandardBetAmount))
    game.dealer.addCard(StandardCard(Suit.Hearts, Value.Ten))
    game.dealer.addCard(StandardCard(Suit.Spades, Value.Ten))
    game.dealer.addCard(StandardCard(Suit.Clubs, Value.Five))
    given mockConsole: Console[IO] = mockConsoleWith(() => "")
    handleHandWinners(game).unsafeRunSync()
    outputMessages.exists(_.contains("DEALER BUSTED")) shouldBe true

  test("Bankrupt or leaving players should be removed."):
    val player3InitialBalance = 0
    val player4InitialBalance = 3
    val player3 = NormalPlayer("P3", player3InitialBalance)
    val player4 = NormalPlayer("P4", player4InitialBalance)
    val game = Game(List(player1, player3, player4))
    val simulatedInputs = Iterator(player1.name)
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    finalizeHand(game).unsafeRunSync()
    game.players shouldBe empty

  test("Remaining players state should reset to active for next round."):
    player1.addCard(StandardCard(Suit.Hearts, Value.Ten))
    player1.bust()
    player2.addCard(StandardCard(Suit.Hearts, Value.King))
    game.dealer.addCard(StandardCard(Suit.Hearts, Value.Nine))
    given mockConsole: Console[IO] = mockConsoleWith(() => "")
    finalizeHand(game).unsafeRunSync()
    game.players.map(_.state) shouldBe List(PlayerState.Active, PlayerState.Active)

  test("All players cards should be cleared after hand ends."):
    player1.addCard(StandardCard(Suit.Hearts, Value.Ten))
    player2.addCard(StandardCard(Suit.Hearts, Value.King))
    game.dealer.addCard(StandardCard(Suit.Hearts, Value.Nine))
    given mockConsole: Console[IO] = mockConsoleWith(() => "")
    finalizeHand(game).unsafeRunSync()
    game.players.map(_.cards) shouldBe List(List.empty, List.empty)

  test("Dealer cards should be cleared after hand ends"):
    player1.addCard(StandardCard(Suit.Hearts, Value.Ten))
    player2.addCard(StandardCard(Suit.Hearts, Value.King))
    game.dealer.addCard(StandardCard(Suit.Hearts, Value.Nine))
    given mockConsole: Console[IO] = mockConsoleWith(() => "")
    finalizeHand(game).unsafeRunSync()
    game.dealer.cards shouldBe empty

  test("The game should end when cut card is drawn."):
    val craftedDeck = Deck.testDeck(
      CutCard,
      StandardCard(Suit.Hearts, Value.Six),
      StandardCard(Suit.Hearts, Value.Ten),
      StandardCard(Suit.Spades, Value.Queen),
      StandardCard(Suit.Clubs, Value.Ten),
      StandardCard(Suit.Spades, Value.Two)
    )
    val testGame = Game(List(player1), craftedDeck)
    val simulatedInputs = Iterator(StandardBetAmount.toString, "S", player1.name)
    given mockConsole: Console[IO] = mockConsoleWith(() => simulatedInputs.next())
    handleHands(testGame).unsafeRunSync()
    testGame.isOver shouldBe true

  test("Game should end immediately if there are no human players left."):
    game.removeLeavingPlayers(List(player1, player2))
    given mockConsole: Console[IO] = mockConsoleWith(() => "")
    handleHands(game).unsafeRunSync()
    game.isOver shouldBe true

  test("Game over message should be printed"):
    given Console[IO] = mockConsoleWith(() => "")
    endGame(game).unsafeRunSync()
    outputMessages.exists(_.contains("The game is over!")) shouldBe true

  test("The final balance of a player should be printed"):
    given Console[IO] = mockConsoleWith(() => "")
    endGame(game).unsafeRunSync()
    outputMessages.exists(_.contains(s"${player1.name} ends the game with $DefaultP1Balance €.")) shouldBe true
    outputMessages.exists(_.contains(s"${player2.name} ends the game with $DefaultP2Balance €.")) shouldBe true