package model

import utils.TestExports.*
import utils.ModelExports.*

import scala.annotation.tailrec

class GameTest extends AnyFunSuite with BeforeAndAfterEach:

  var firstPlayer: NormalPlayer = _
  var secondPlayer: NormalPlayer = _
  var listPlayers: List[Player] = _
  val splitCard: StandardCard = StandardCard(Suit.Hearts, Value.Ace)
  val splitPlayer = SplitPlayer("Tina", splitCard)
  var game: Game = _
  val initialDeckSize: Int = Game.NumDecks * 52 + 1
  val betAmount = 100
  val BlackjackPayoutMultiplier = 2.5
  val ace: StandardCard = StandardCard(Suit.Hearts, Value.Ace)
  val king: StandardCard = StandardCard(Suit.Spades, Value.King)
  val ten: StandardCard = StandardCard(Suit.Hearts, Value.Ten)
  val six: StandardCard = StandardCard(Suit.Spades, Value.Six)
  val eight: StandardCard = StandardCard(Suit.Clubs, Value.Eight)
  val hiddenCard: StandardCard = StandardCard(Suit.Hearts, Value.Queen, isFaceUp = false)
  val hiddenAceCard: StandardCard = StandardCard(Suit.Clubs, Value.Ace, isFaceUp = false)

  override def beforeEach(): Unit =
    firstPlayer = NormalPlayer("Alice", 200)
    secondPlayer = NormalPlayer("Bob", 300)
    listPlayers = List(firstPlayer, secondPlayer)
    game = Game(listPlayers)

  /**Helper function to add a sequence of cards to the participant's hand*/
  def giveCards(participant: Participant)(cards: StandardCard*): Unit =
    cards.foreach(card => participant.addCard(card))

  test("isPlayerNumValid should reject numbers below the minimum limit"):
    Game.isPlayerNumValid(0) shouldBe false

  test("isPlayerNumValid should accept numbers within the standard range"):
    Game.isPlayerNumValid(1) shouldBe true
    Game.isPlayerNumValid(7) shouldBe true

  test("isPlayerNumValid should reject numbers above the maximum limit"):
    Game.isPlayerNumValid(8) shouldBe false

  test("arePlayersNamesValid should accept a correct list of unique names"):
    val expectedCount = 3
    val validNames = List("Elena", "Chiara", "Marco")
    Game.arePlayersNamesValid(expectedCount)(validNames) shouldBe true

  test("arePlayersNamesValid should fail when the number of names doesn't match the expected count"):
    val expectedCount = 3
    val tooFewNames = List("Elena", "Chiara")
    Game.arePlayersNamesValid(expectedCount)(tooFewNames) shouldBe false

  test("arePlayersNamesValid should fail when duplicate names are provided"):
    val expectedCount = 3
    val duplicateNames = List("Elena", "Chiara", "Elena")
    Game.arePlayersNamesValid(expectedCount)(duplicateNames) shouldBe false

  test("arePlayersNamesValid should fail when a name contains an underscore"):
    val expectedCount = 3
    val invalidCharNames = List("Elena", "Chiara_Bot", "Marco")
    Game.arePlayersNamesValid(expectedCount)(invalidCharNames) shouldBe false

  test("arePlayersNamesValid should fail when a name is empty"):
    val expectedCount = 3
    val emptyNameList = List("Elena", "", "Marco")
    Game.arePlayersNamesValid(expectedCount)(emptyNameList) shouldBe false

  test("isInitialDepositValid should accept standard multiple amounts"):
    Game.isInitialDepositValid(50.0) shouldBe true

  test("isInitialDepositValid should accept valid decimal alignment"):
    Game.isInitialDepositValid(12.5) shouldBe true

  test("isInitialDepositValid should reject non-aligned cents amounts"):
    Game.isInitialDepositValid(12.25) shouldBe false

  test("isInitialDepositValid should reject negative deposit amounts"):
    Game.isInitialDepositValid(-10.0) shouldBe false

  test("isNameValid should return true if the player name exists in the game"):
    game.isNameValid("Alice") shouldBe true
    game.isNameValid("Chiara") shouldBe false

  test("isNameValid should be case sensitive when validating names"):
    game.isNameValid("alice") shouldBe false

  test("isBetValid should accept bets that are multiples of minBet and within player balance"):
    game.isBetValid(firstPlayer)(5.0) shouldBe true
    game.isBetValid(firstPlayer)(100.0) shouldBe true

  test("isBetValid should reject bets that are negative, zero, not multiples of minBet, or exceed balance"):
    game.isBetValid(firstPlayer)(0.0) shouldBe false
    game.isBetValid(firstPlayer)(-5.0) shouldBe false
    game.isBetValid(firstPlayer)(7.2) shouldBe false
    game.isBetValid(firstPlayer)(500.0) shouldBe false

  test("player's bet is computed correctly"):
    val playerBet = Bet(firstPlayer, betAmount)
    playerBet.player shouldBe firstPlayer
    playerBet.amount shouldBe betAmount

  test("game is initialized as expected"):
    game.players shouldBe listPlayers
    game.currentBets shouldBe List.empty
    game.deck.size() shouldBe initialDeckSize

  test("balances should correctly map players to tuples of (name, totalValue)"):
    val result = game.balances(List(firstPlayer, secondPlayer))
    result shouldBe List(("Alice", 200.0), ("Bob", 300.0))

  test("balances should return an empty list when given an empty list of players"):
    val result = game.balances(List.empty)
    result shouldBe List.empty

  test("players' bets are set correctly"):
    val bets = List(Bet(firstPlayer, betAmount), Bet(secondPlayer, betAmount))
    game.currentBets = bets
    game.currentBets.length shouldBe bets.length
    game.currentBets shouldBe bets

  test("distributeCards should give exactly 2 cards to each participant (players + dealer)"):
    val numParticipants = 3
    game.players.foreach(p => p.cards shouldBe List.empty)
    game.dealer.cards shouldBe empty
    val initialDeckSize = game.deck.size()
    val messages = game.distributeCards()
    game.players.foreach(p => p.cards.size shouldBe 2)
    game.dealer.cards.size shouldBe 2
    game.dealer.cards.head.isFaceUp shouldBe true
    game.dealer.cards(1).isFaceUp shouldBe false
    game.deck.size() shouldBe (initialDeckSize - numParticipants * 2)

  test("distributeCards should return a list of formatted strings for each card dealt"):
    val expectedMessageCount = game.players.size * 3
    val messages = game.distributeCards()
    messages.size shouldBe expectedMessageCount
    messages.count(_.contains("Bob")) shouldBe 2
    messages.count(_.contains("Alice")) shouldBe 2
    messages.count(_.contains("Dealer")) shouldBe 2

  test("dealerHasAce should return true when the dealer has a face-up Ace"):
    giveCards(game.dealer)(ace, hiddenCard)
    game.dealerHasAce shouldBe true

  test("dealerHasAce should return false when the dealer has an Ace but it is face-down"):
    giveCards(game.dealer)(hiddenCard, six)
    game.dealerHasAce shouldBe false
    
  test("dealerHasAce should return false when the dealer has no Aces at all"):
    val hiddenQueenCard = hiddenCard
    giveCards(game.dealer)(king, hiddenQueenCard)
    game.dealerHasAce shouldBe false

  test("players are correctly removed from the game"):
    game.removeLeavingPlayers(List(firstPlayer))
    val remainingPlayers = List(secondPlayer)
    game.players.length shouldBe remainingPlayers.length
    game.players shouldEqual remainingPlayers

  test("game does not terminate if there are still active players"):
    game.isOver shouldBe false

  test("game terminates correctly when no players are left"):
    game.addBots()
    game.removeLeavingPlayers(List(firstPlayer, secondPlayer))
    game.isOver shouldBe true

  test("playersWithBlackjack returns the players who got a natural blackjack"):
    giveCards(firstPlayer)(ace, king)
    giveCards(secondPlayer)(ten, six)
    game.initialBlackjackPlayers() shouldBe List(firstPlayer)

  test("playersWithBlackjack returns an empty list when no player has blackjack"):
    giveCards(firstPlayer)(ten, six)
    giveCards(secondPlayer)(ten, six)
    game.initialBlackjackPlayers() shouldBe List.empty

  test("handleBlackjacks pays the winners instantly (if the dealer can't has BJ) with the bet multiplied by the blackjack payout"):
    val startingBalance = firstPlayer.balance.totalValue
    game.currentBets = List(Bet(firstPlayer, betAmount), Bet(secondPlayer, betAmount))
    giveCards(game.dealer)(six)
    giveCards(firstPlayer)(ace, king)
    giveCards(secondPlayer)(ten, six)
    game.handleBlackjacks(game.initialBlackjackPlayers())
    firstPlayer.balance.totalValue shouldBe startingBalance + betAmount * BlackjackPayoutMultiplier

  test("handleBlackjacks updates the state of every player with BJ to 'Blackjack' also if the dealer could have BJ without adding immediately the winnings"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    val firstPlayerInitialBalance = firstPlayer.balance.totalValue
    giveCards(game.dealer)(ace)
    giveCards(firstPlayer)(ace, king)
    game.handleBlackjacks(List(firstPlayer))
    firstPlayer.state shouldBe PlayerState.Blackjack
    firstPlayer.balance.totalValue shouldBe firstPlayerInitialBalance

  test("handleBlackjacks does not affect players that are not winners"):
    val secondPlayerBalance = secondPlayer.balance.totalValue
    game.currentBets = List(Bet(firstPlayer, betAmount), Bet(secondPlayer, betAmount))
    giveCards(game.dealer)(six)
    giveCards(firstPlayer)(ace, king)
    giveCards(secondPlayer)(king, six)
    game.handleBlackjacks(game.initialBlackjackPlayers())
    secondPlayer.balance.totalValue shouldBe secondPlayerBalance
    secondPlayer.state shouldBe PlayerState.Active

  test("drawCard should add a card to the participant and update the deck when cards are available"):
    val initialDeckSize = game.deck.size()
    val drawnCardOpt = game.drawCard(firstPlayer)
    drawnCardOpt shouldBe defined
    firstPlayer.cards.size shouldBe 1
    firstPlayer.cards.head shouldBe drawnCardOpt.get
    game.deck.size() shouldBe (initialDeckSize - 1)

  test("A player that is busted should be detected correctly by the game"):
    val bustedHand = List(ten, king, six)
    giveCards(firstPlayer)(bustedHand: _*)
    game.evaluatePlayerBust(firstPlayer) shouldBe true
    firstPlayer.state shouldBe PlayerState.Busted

  test("Dealer should be seen as busted only if its score is bigger then 21"):
    giveCards(game.dealer)(ten, six)
    game.evaluateDealerBust shouldBe false
    giveCards(game.dealer)(ten)
    game.evaluateDealerBust shouldBe true

  test("Bot draws cards until reaching its finished turn threshold"):
    val bot = BotPlayer(name = "TestBot", initialBalance = 100.0, bet = 10)
    giveCards(bot)(ten, six)
    val game = Game(List(bot))
    val initialCards = bot.cards.size
    val messages = game.computeBotTurn(bot)
    bot.cards.size should be > initialCards
    bot.hasFinishedTurn shouldBe true
    bot.cards.calculateScore.maxValue should be >= 17
    messages shouldNot be(empty)

  test("Bot does not draw cards when it has already finished its turn (score >= 17)"):
    val bot = BotPlayer(name = "TestBot", initialBalance = 100.0, bet = 10)
    giveCards(bot)(king, ten)
    val game = Game(List(bot))
    val initialCards = bot.cards.size
    val messages = game.computeBotTurn(bot)
    bot.cards.size shouldBe initialCards
    bot.hasFinishedTurn shouldBe true
    bot.cards.calculateScore.maxValue shouldBe 20

  test("computeDealerTurn keeps drawing when the high value busts but the low still below the standing threshold"):
    giveCards(game.dealer)(hiddenAceCard, six, six)
    game.computeDealerTurn()
    game.dealer.score.playableValue should be >= 17

  test("After the turn of the Dealer the second drawn card should be visible"):
    giveCards(game.dealer)(ten, hiddenCard)
    game.computeDealerTurn()
    val numDealerCards = game.dealer.cards.size
    game.dealer.cards.count(c => c.isFaceUp) shouldBe numDealerCards

  test("Dealer draws cards until reaching at least 17"):
    giveCards(game.dealer)(ten, six)
    val initialCards = game.dealer.cards.size
    val messages = game.computeDealerTurn()
    game.dealer.cards.size should be > initialCards
    game.dealer.cards.calculateScore.maxValue should be >= 17

  test("Dealer does not draw cards when score is already 17 or higher"):
    giveCards(game.dealer)(ten, king)
    val initialCards = game.dealer.cards.size
    val messages = game.computeDealerTurn()
    game.dealer.cards.size shouldBe initialCards
    game.dealer.cards.calculateScore.maxValue shouldBe 20

  test("startNewHand clears hands of players and dealer"):
    giveCards(game.dealer)(king)
    giveCards(firstPlayer)(ten)
    giveCards(secondPlayer)(six)
    game.handleHandEnd()
    game.dealer.cards.size shouldBe 0
    firstPlayer.cards.size shouldBe 0
    secondPlayer.cards.size shouldBe 0

  test("drawStandardCard should catch CutCard, set isCutCardInDeck to false, and recursively return a StandardCard"):
   game.isCutCardInDeck shouldBe true
    @tailrec
    def flushUntilCutCard(): Unit =
      if game.isCutCardInDeck then
        game.drawStandardCard()
        flushUntilCutCard()
    flushUntilCutCard()
    game.isCutCardInDeck shouldBe false

  test("drawCard should transparently give a StandardCard to the player even if a CutCard is skipped underneath"):
    while game.isCutCardInDeck do
      game.drawCard(firstPlayer)
    firstPlayer.cards.forall(_.isInstanceOf[Card.StandardCard]) shouldBe true

  test("canDoubleDown is true when the player has exactly two cards and enough balance"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    giveCards(firstPlayer)(ten, six)
    game.canDoubleDown(firstPlayer) shouldBe true

  test("canDoubleDown is false when the player has only one card"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    firstPlayer.addCard(six)
    game.canDoubleDown(firstPlayer) shouldBe false

  test("canDoubleDown is false when the player has more than two cards"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    giveCards(firstPlayer)(ten, six, king)
    game.canDoubleDown(firstPlayer) shouldBe false

  test("canDoubleDown is false when the player doesn't have enough balance"):
    val notDoubleDownableBet = firstPlayer.balance.totalValue.toInt + 1
    game.currentBets = List(Bet(firstPlayer, notDoubleDownableBet))
    giveCards(firstPlayer)(ten, six)
    game.canDoubleDown(firstPlayer) shouldBe false

  test("doubleDown doubles the player's current bet"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    giveCards(firstPlayer)(ten, six)
    game.doubleDown(firstPlayer)
    game.currentBets shouldBe List(Bet(firstPlayer, betAmount * 2))

  test("doubleDown draws exactly one card for the player"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    giveCards(firstPlayer)(ten, six)
    game.doubleDown(firstPlayer)
    firstPlayer.cards.size shouldBe 3

  test("The split can be done if the player has two cards of the same value"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    giveCards(firstPlayer)(ten, ten)
    game.canSplit(firstPlayer) shouldBe true

  test("The split cannot be done if the player has multiple cards or two cards with different value"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    giveCards(firstPlayer)(ten, ten, ten)
    game.canSplit(firstPlayer) shouldBe false
    firstPlayer.clearHand()
    giveCards(firstPlayer)(ace, ten)
    game.canSplit(firstPlayer) shouldBe false

  test("The split cannot be done if the player does not have enough balance"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    val bet = 100
    firstPlayer.withdraw(bet)
    game.canSplit(firstPlayer) shouldBe false

  test("A split player should have the split card in its hand"):
    splitPlayer.cards.size shouldBe 1
    splitPlayer.balance.totalValue shouldBe 0.0
    splitPlayer.cards should contain(splitCard)

  test("A split player cannot split its card if it has an ace"):
    game.currentBets = List(Bet(splitPlayer, betAmount))
    splitPlayer.deposit(2 * betAmount)
    splitPlayer.addCard(ace)
    game.canSplit(splitPlayer) shouldBe false

  test("splitPlayer should create a SplitPlayer"):
    game.currentBets = List(Bet(firstPlayer, 30))
    giveCards(firstPlayer)(six, six)
    val result = game.splitPlayer(firstPlayer)
    result should not be empty
    game.players.size shouldBe 3
    val splitPlayer = game.players.find(_.isInstanceOf[SplitPlayer])
    splitPlayer should not be empty
    splitPlayer.get.name shouldBe firstPlayer.name + "_split1"

  test("splitPlayer should create a new bet for the SplitPlayer"):
    val expectedBet = 30
    game.currentBets = List(Bet(firstPlayer, expectedBet))
    giveCards(firstPlayer)(six, six)
    game.splitPlayer(firstPlayer)
    val splitPlayerBet = game.currentBets.find(_.player.name == firstPlayer.name + "_split1")
    splitPlayerBet.get.amount shouldBe expectedBet

  test("splitPlayer should deduct the bet amount from the original player balance"):
    val expectedBet = 30
    val playerInitialBalance = firstPlayer.balance.totalValue
    game.currentBets = List(Bet(firstPlayer, expectedBet))
    giveCards(firstPlayer)(six, six)
    game.splitPlayer(firstPlayer)
    firstPlayer.balance.totalValue shouldBe playerInitialBalance - expectedBet

  test("splitPlayer should draw one card for both players"):
    game.currentBets = List(Bet(firstPlayer, 30))
    giveCards(firstPlayer)(six, six)
    val result = game.splitPlayer(firstPlayer)
    val splitPlayer = game.players.find(_.isInstanceOf[SplitPlayer]).get
    firstPlayer.cards.size shouldBe 2
    splitPlayer.cards.size shouldBe 2
    result.get._1 shouldBe firstPlayer.cards.last
    result.get._2 shouldBe splitPlayer.cards.last

  test("The splitting should not be done if the player has two ace and had already perform a split before"):
    val splitPlayer = SplitPlayer(firstPlayer.name + "_split1", ace)
    val testGame = Game(List(firstPlayer, splitPlayer))
    giveCards(firstPlayer)(ace, ace)
    giveCards(splitPlayer)(ace)
    game.currentBets = List(Bet(firstPlayer, 20), Bet(splitPlayer, 20))
    game.canSplit(firstPlayer) shouldBe false
    game.canSplit(splitPlayer) shouldBe false

  test("The splitting should not be done if the player has two different cards even if the first one is ace"):
    val ace: StandardCard = StandardCard(Suit.Hearts, Value.Ace)
    val testGame = Game(List(firstPlayer))
    giveCards(firstPlayer)(ace, six)
    giveCards(splitPlayer)(ace)
    game.currentBets = List(Bet(firstPlayer, 20))
    game.canSplit(firstPlayer) shouldBe false

  test("If a player that decides to split has chosen the insurance option the bet of the split player should be equal to the initial bet (not considering the additional part for the bet)"):
    val initialBetPercentage: Double = 2.0 / 3.0
    val expectedSplitPlayerBetAmount = (betAmount * initialBetPercentage).toInt
    game.currentBets = List(Bet(firstPlayer, betAmount))
    giveCards(firstPlayer)(ace, ace)
    firstPlayer.hasInsurance = true
    game.splitPlayer(firstPlayer)
    val splitPlayer = game.players(1)
    val splitPlayerBetAmount = game.currentBets.find(_.player == splitPlayer).get.amount
    splitPlayerBetAmount shouldBe expectedSplitPlayerBetAmount

  test("transferBalance should transfer the player's balance to the first SplitPlayer"):
    val splitPlayer = SplitPlayer(firstPlayer.name + "_split1", ace)
    val testGame = Game(List(firstPlayer, splitPlayer))
    val expectedBalance = firstPlayer.balance.totalValue
    testGame.transferBalance(firstPlayer)
    firstPlayer.balance.totalValue shouldBe 0.0
    splitPlayer.balance.totalValue shouldBe expectedBalance

  test("transferBalance should transfer the SplitPlayer's balance to the next SplitPlayer"):
    val expectedBalance = 100
    val splitPlayer = SplitPlayer(firstPlayer.name + "_split1", ace)
    splitPlayer.deposit(expectedBalance)
    val secondSplit = SplitPlayer(firstPlayer.name + "_split2", ace)
    val testGame = Game(List(firstPlayer, splitPlayer, secondSplit))
    testGame.transferBalance(splitPlayer)
    splitPlayer.balance.totalValue shouldBe 0.0
    secondSplit.balance.totalValue shouldBe expectedBalance

  test("transferBalance should not transfer balance when the player has no next SplitPlayer"):
    val splitPlayer = SplitPlayer(firstPlayer.name + "_split1", ace)
    val testGame = Game(List(firstPlayer, splitPlayer, secondPlayer))
    val secondPlayerBalance = secondPlayer.balance.totalValue
    testGame.transferBalance(secondPlayer)
    secondPlayer.balance.totalValue shouldBe secondPlayerBalance

  test("transfer balance should not change the initial balance of players in case of no splits"):
    val testGame = Game(List(firstPlayer, secondPlayer))
    val expectedBalance1 = firstPlayer.balance.totalValue
    val expectedBalance2 = secondPlayer.balance.totalValue
    testGame.transferBalance(firstPlayer)
    testGame.transferBalance(secondPlayer)
    firstPlayer.balance.totalValue shouldBe expectedBalance1
    secondPlayer.balance.totalValue shouldBe expectedBalance2

  test("payOutHand pays the player double the bet when their score beats a non-busted dealer"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    giveCards(game.dealer)(ten, six)
    giveCards(firstPlayer)(ten, eight)
    val startingBalance = firstPlayer.balance.totalValue
    game.handlePayout()
    firstPlayer.balance.totalValue shouldBe startingBalance + betAmount * 2

  test("payOutHand return the bet to the player when he push"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    giveCards(game.dealer)(ten, eight)
    giveCards(firstPlayer)(ten, eight)
    val startingBalance = firstPlayer.balance.totalValue
    game.handlePayout()
    firstPlayer.balance.totalValue shouldBe startingBalance + betAmount

  test("payOutHand takes the bet when the player loses to a higher dealer score"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    giveCards(game.dealer)(ten, ten)
    giveCards(firstPlayer)(ten, eight)
    val startingBalance = firstPlayer.balance.totalValue
    game.handlePayout()
    firstPlayer.balance.totalValue shouldBe startingBalance

  test("payOutHand pays the player when the dealer busts, regardless of the player's own score"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    val bustedHand = List(ten, ten, six)
    giveCards(game.dealer)(bustedHand: _*)
    giveCards(firstPlayer)(six, six)
    val startingBalance = firstPlayer.balance.totalValue
    game.handlePayout()
    firstPlayer.balance.totalValue shouldBe startingBalance + betAmount * 2

  test("payOutHand takes the bet from a busted player even when the dealer also busts"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    val bustedHand1 = List(ten, ten, six)
    val bustedHand2 = List(ten, king, six)
    giveCards(game.dealer)(bustedHand1: _*)
    giveCards(firstPlayer)(bustedHand2: _*)
    game.evaluatePlayerBust(firstPlayer) shouldBe true
    val startingBalance = firstPlayer.balance.totalValue
    game.handlePayout()
    firstPlayer.balance.totalValue shouldBe startingBalance

  test("payOutHand credits the blackjack payout when the player has blackjack against a 21"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    val five: StandardCard = StandardCard(Suit.Clubs, Value.Five)
    giveCards(game.dealer)(ten, six, five)
    giveCards(firstPlayer)(ace, king)
    firstPlayer.winBlackjack()
    val startingBalance = firstPlayer.balance.totalValue
    game.handlePayout()
    firstPlayer.balance.totalValue shouldBe startingBalance + betAmount * BlackjackPayoutMultiplier

  test("payOutHand pushes between two BJ"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    giveCards(game.dealer)(ace, king)
    giveCards(firstPlayer)(ace, king)
    firstPlayer.winBlackjack()
    val startingBalance = firstPlayer.balance.totalValue
    game.handlePayout()
    firstPlayer.balance.totalValue shouldBe startingBalance + betAmount

  test("payOutHand makes the player lose against a BJ even if he has 21"):
    val five: StandardCard = StandardCard(Suit.Clubs, Value.Five)
    game.currentBets = List(Bet(firstPlayer, betAmount))
    giveCards(game.dealer)(ace, king)
    giveCards(firstPlayer)(ace, five, five)
    val startingBalance = firstPlayer.balance.totalValue
    game.handlePayout()
    firstPlayer.balance.totalValue shouldBe startingBalance

  test("payOutHand increases the dealer's profit by the lost bet when the player loses"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    giveCards(game.dealer)(ten, ten)
    giveCards(firstPlayer)(ten, eight)
    val startingProfit = game.dealer.totalProfit
    game.handlePayout()
    game.dealer.totalProfit shouldBe startingProfit + betAmount

  test("payOutHand decreases the dealer's profit by the amount paid out when the player wins"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    giveCards(game.dealer)(ten, six)
    giveCards(firstPlayer)(ten, eight)
    val startingProfit = game.dealer.totalProfit
    game.handlePayout()
    game.dealer.totalProfit shouldBe startingProfit - betAmount

  test("payOutHand does handle a player who has already been paid for their blackjack"):
    game.currentBets = List(Bet(secondPlayer, betAmount))
    giveCards(game.dealer)(ten, six)
    giveCards(firstPlayer)(ace, king)
    firstPlayer.winBlackjack()
    val Balance = firstPlayer.balance.totalValue
    game.handlePayout()
    firstPlayer.balance.totalValue shouldBe Balance

  test("payOutHand should credit the split hand's winnings and its remaining balance to the original player"):
    val splitPlayer = SplitPlayer(s"${firstPlayer.name}_1", ace, betAmount)
    game = Game(List(firstPlayer, splitPlayer))
    game.currentBets = List(Bet(firstPlayer, betAmount), Bet(splitPlayer, betAmount))
    giveCards(game.dealer)(ten, six)
    giveCards(firstPlayer)(ten, StandardCard(Suit.Clubs, Value.Five))
    giveCards(splitPlayer)(eight)
    val startingBalance = firstPlayer.balance.totalValue
    game.handlePayout()
    firstPlayer.balance.totalValue shouldBe startingBalance + betAmount * 3

  test("payOutHand handles scenario where both original and split hands win against the dealer"):
    val splitPlayer = SplitPlayer(s"${firstPlayer.name}_1", ace, betAmount)
    game = Game(List(firstPlayer, splitPlayer))
    game.currentBets = List(Bet(firstPlayer, betAmount), Bet(splitPlayer, betAmount))
    giveCards(game.dealer)(ten, StandardCard(Suit.Spades, Value.Seven))
    giveCards(firstPlayer)(ten, eight)
    giveCards(splitPlayer)(eight)
    val startingBalance = firstPlayer.balance.totalValue
    game.handlePayout()
    firstPlayer.balance.totalValue shouldBe startingBalance + (betAmount * 5)

  test("payOutHand updates dealer profit correctly considering multiple bets from a split player"):
    val splitPlayer = SplitPlayer(s"${firstPlayer.name}_1", ace, betAmount)
    game = Game(List(firstPlayer, splitPlayer))
    game.currentBets = List(Bet(firstPlayer, betAmount), Bet(splitPlayer, betAmount))
    giveCards(game.dealer)(ten, ten)
    giveCards(firstPlayer)(ten, StandardCard(Suit.Clubs, Value.Seven))
    giveCards(splitPlayer)(six)
    val startingBalance = firstPlayer.balance.totalValue
    game.handlePayout()
    firstPlayer.balance.totalValue shouldBe startingBalance + betAmount

  test("A split player should be correctly removed from the list of players."):
    val splitPlayer = SplitPlayer(s"${firstPlayer.name}_1", ace, betAmount)
    game = Game(List(firstPlayer, splitPlayer))
    game.removeSplitPlayers()
    game.players.size shouldBe 1
    game.players shouldEqual List(firstPlayer)

  test("handleInsurances should update the bet of insured players"):
    val prevBet = Bet(firstPlayer, betAmount)
    val expectedBet = Bet(firstPlayer, betAmount + betAmount / 2)
    game.currentBets = List(prevBet, Bet(secondPlayer, betAmount))
    game.handleInsurances(List(firstPlayer.name))
    game.currentBets should not contain prevBet
    game.currentBets should contain(expectedBet)
    game.players.head.asInstanceOf[NormalPlayer].hasInsurance shouldBe true
    game.players(1).asInstanceOf[NormalPlayer].hasInsurance shouldBe false

  test("resolveInsurances pays out, restores the bet, and returns the wins when the dealer has blackjack"):
    val insuredBet = betAmount + betAmount / 2
    firstPlayer.hasInsurance = true
    game.currentBets = List(Bet(firstPlayer, insuredBet))
    giveCards(game.dealer)(ace, king)
    val startingBalance = firstPlayer.balance.totalValue
    game.resolveInsurances() shouldBe List((firstPlayer.name, betAmount.toDouble))
    game.currentBets shouldBe List(Bet(firstPlayer, betAmount))
    firstPlayer.balance.totalValue shouldBe startingBalance + betAmount

  test("resolveInsurances does not pay and restores the bet when the dealer does not have blackjack"):
    val insuredBet = betAmount + betAmount / 2
    firstPlayer.hasInsurance = true
    game.currentBets = List(Bet(firstPlayer, insuredBet))
    giveCards(game.dealer)(ace, six)
    val startingBalance = firstPlayer.balance.totalValue
    game.resolveInsurances() shouldBe List.empty
    game.currentBets shouldBe List(Bet(firstPlayer, betAmount))
    firstPlayer.balance.totalValue shouldBe startingBalance

  test("resolveInsurances does not affect players without insurance"):
    game.currentBets = List(Bet(firstPlayer, betAmount))
    giveCards(game.dealer)(ace, king)
    game.resolveInsurances()
    game.currentBets shouldBe List(Bet(firstPlayer, betAmount))

  test("addBots should fill the game with bots up to MaxPlayersNum using existing players"):
    game.addBots()
    game.players.size shouldBe Game.MaxPlayersNum
    game.players(2).name shouldBe "Bot1"
    game.players(3).name shouldBe "Bot2"
    game.players(2) shouldBe a[BotPlayer]
    game.players(3) shouldBe a[BotPlayer]

  test("addBots should not add any bots if the game is already at MaxPlayersNum"):
    game.addBots()
    game.addBots()
    game.players.size shouldBe Game.MaxPlayersNum
