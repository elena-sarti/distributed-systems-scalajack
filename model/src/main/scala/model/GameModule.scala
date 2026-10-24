package model

import scala.annotation.tailrec

object GameModule:
  import utils.ModelExports.*

  /** Represents a bet placed by a player in the game.
   *
   * @param player The Player who is placing the bet.
   * @param amount The amount of coins wagered by the player.
   */
  case class Bet(player: Player, amount: Int)

  trait Game:

    /** Returns the list of players currently participating in the game.
     *
     * @return the list of players in the game.
     */
    def players: List[Player]

    /** Generates a list of BotPlayers and appends them to the current players list to have a number of participant equal
     * to the maximum possible number. */
    def addBots(): Unit

    /** Returns a list containing for name and current balance of each player specified in input.
     *
     * @param players The list of players which the output refers to.
     * @return A list of tuples containing the name and balance of each player.
     */
    def balances(players: List[Player]): List[(String, Double)]

    /** Returns the dealer of the game.
     *
     * @return The dealer currently participating in the game.
     */
    def dealer: Dealer

    /** Returns the current deck of the game */
    def deck: Deck

    /** Returns the list of bets placed during the current round.
     *
     * @return The current list of bets.
     */
    def currentBets: List[Bet]

    /** Updates or replaces the list of bets for the current round.
     *
     * @param bets The new list of player's bets to be applied to the game.
     */
    def currentBets_=(bets: List[Bet]): Unit

    /** Checks if a given bet is valid.
     *
     * @param player The player making the bet.
     * @param amount The amount that he wants to bet
     * @return [[true]] if the bet is valid, [[false]] if it is not.
     */
    def isBetValid(player: Player)(amount: Double): Boolean

    /** Checks is a given name is valid, meaning it is one of the players' names.
     * 
     * @param name The name to be checked.
     * @return [[true]] if the name is valid, [[false]] if it is not.
     */
    def isNameValid(name: String): Boolean

    /** Draws cards from the deck until a standard card is drawn. Each card
     * drawn is removed from the deck.
     *
     * @return An Optional containing the standard card drawn from the desk if not empty
     */
    def drawStandardCard(): Option[StandardCard]

    /** Draws a standard card from the desk and adds it to the player's list of cards.
     *
     * @param participant The participant asking for a new card
     * @return An Optional containing the standard card drawn from the desk if not empty
     */
    def drawCard(participant: Participant): Option[Card]

    /** Distributes two cards to each player, one card at a time
     *
     * @return a list containing the value to print in standard output for every card that has been distributed
     */
    def distributeCards(): List[String]

    /** Returns the players who got a BJ right after the initial deal (two cards with sum equal to 21).
     *
     * @return the list of players who have blackjack in the current round (can be empty).
     */
    def initialBlackjackPlayers(): List[Player]

    /** Returns the players in the given list whose current hand is a blackjack.
     * This method is useful for checking blackjack after game events such as a split,
     * where only a subset of players must be examined.
     *
     * @param players the players to check.
     * @return the players whose current hand is a blackjack.
     */
    def playersWithBlackjack(players: List[Player]): List[Player]

    /** Handles every player in the given list who got a blackjack: credits their balance with their
     *  current bet multiplied by the blackjack payout, and updates their state to `Blackjack`.
     *
     * @param winners The players to handle for their blackjack.
     */
    def handleBlackjacks(winners: List[Player]): Unit

    /**
     * Handles the players who chose the insurance option by increasing
     * their current bet by half of the original bet.
     *
     * @param names the list of player names who opted for insurance.
     */
    def handleInsurances(names: List[String]): Unit

    /** Resolves the insurance bets placed by insured players once the dealer's hand is revealed.
     *
     * For each player who has insurance, the insurance is removed from the
     * current bet, restoring it to the original amount. If the dealer has blackjack,
     * players with insurance get paid out.
     *
     * @return a list of tuples containing, for each player with insurance, their name and the amount
     *         won from the insurance (zero if the dealer did not have a blackjack).
     */
    def resolveInsurances(): List[(String, Double)]

    /** Checks whether a specified player is busted and updates its state accordingly.
     *
     * @param player The player to be checked.
     * @return true if the player is busted, false otherwise.
     */
    def evaluatePlayerBust(player: Player): Boolean

    /** Checks whether the dealer is busted
     *
     * @return true if the dealer is busted, false otherwise
     */
    def evaluateDealerBust: Boolean
    
    /** Checks whether the dealer has an Ace face up.
     * 
     * @return [[true]] if the dealer has an Ace, [[false]] otherwise
     * */
    def dealerHasAce: Boolean

    /** Checks if the given player can double down, allowed only when the
     * player has exactly two cards in hand, and has the balance to do that.
     *
     * @param player is the checked player
     * @return [[true]] if the double down for the player is possible, [[false]] otherwise
     */
    def canDoubleDown(player: Player): Boolean

    /** Checks whether the player can perform a split.
     *
     * A player can split if they have exactly two cards with the same value and enough balance.
     *
     * @param player is the checked player
     * @return [[true]] if the split for the player is possible, [[false]] otherwise
     */
    def canSplit(player: Player): Boolean

    /** Doubles the given player's current bet and draw a single card to the player.
     *
     * @param player The player doubling down.
     * @return An Optional containing the card drawn for the player, if the deck was not empty.
     */
    def doubleDown(player: Player): Option[Card]

    /**Splits the given player's hand into two separate hands.
     *
     * Creates a new [[SplitPlayer]] containing one of the cards from the original
     * player's hand and adds it to the game. The original player keeps the remaining
     * card, and both players can continue playing their turns independently.
     *
     * @param player the player who wants to split their hand.
     * @return An Optional containing the cards drawn for the player if not empty
     */
    def splitPlayer(player: Player): Option[(Card, Card)]

    /** Returns the player who takes the turn immediately after the given player.
     *
     * @param player the player whose successor is requested.
     * @return An Option containing player immediately following the given player in the game's turn order if not empty
     */
    def getNextPlayer(player: Player): Option[Player]

    /** Transfers the remaining balance of a player to their corresponding split player.
     *
     * If the player is a split player, the original player's name is extracted.
     * The method checks whether the next player in the turn order belongs to the same
     * original player (i.e. it is a split hand) and, if so, moves the entire balance
     * to that player.
     *
     * @param player The player whose balance must be transferred.
     */
    def transferBalance(player: Player): Unit

    /** Executes the bot's turn following the same logic of the dealer.
     *
     * @param bot The bot on which the actions will be executed.
     * @return    A list containing the value to print in standard output for every card that has been distributed.
     */
    def computeBotTurn(bot: BotPlayer): List[String]

    /** Executes the dealer's turn according to blackjack rules.
     *
     * The dealer first reveals the hidden card and then repeatedly draws cards
     * until reaching a score of at least 17. The dealer does not make choices:
     * cards are automatically drawn following the game rules.
     *
     * @return A list containing the value to print in standard output for every card that has been distributed.
     */
    def computeDealerTurn(): List[String]

    /** Handles payout: pays 1:1 the players whose hand beats the dealer's (including when the dealer busts),
     * returns the bet on a "push" (equal scores), and credits the dealer's profit for every bet lost.
     * Busted players always lose regardless of the dealer's hand.
     * A blackjack (21 with 2 cards) wins over a 21 with more than 2 cards.
     * A 21 with more than 2 cards always "push" a 21 with more than 2 cards, also if the number of cards is not the same.
     */
    def handlePayout(): Unit

    /** Return the players whose balance is minor than the minimum possible bet.
     *
     * @return The list of players that do not have a sufficient balance to continue the game.
     */
    def brokePlayers(): List[Player]

    /** Prepares the game for a new hand: resets every remaining player's state
     * to `Active` and clears their hand, clears the dealer's hands.
     */
    def handleHandEnd(): Unit

    /** Removes all the split players from the current list of players. */
    def removeSplitPlayers(): Unit

    /** Removes all broke players from the current list of players. */
    def removeBrokePlayers(): Unit

    /** Removes all players that want to leave the game from the current list of players.
     *
     * @param leavingPlayers The list of player wishing to leave the game.
     */
    def removeLeavingPlayers(leavingPlayers: List[Player]): Unit

    /** Checks whether the cut card is still in deck
     *
     * @return [[true]] it is, [[false]] otherwise
     */
    def isCutCardInDeck: Boolean

    /**Checks whether the cut card has been extracted and not yet shown on the uer interface.
     *
     * @return [[true]] if the cut card should be shown, [[false]] if it already has.
     */
    def shouldShowCutCardMessage: Boolean

    /** Checks whether human players are still in the game.
     *
     * @return [[true]] if there are no human players left, [[false]] otherwise.
     */
    def areAllHumanPlayersOut: Boolean

    /** Checks if the game is over, meaning that every starting player has already left the table.
     *
     * @return [[true]] if all players have left, [[false]] if there are still active players.
     */
    def isOver: Boolean

  object Game:
    val MinPlayersNum = 1
    val MaxPlayersNum = 7
    val MinGameBet = Fiche.Five.value.toInt
    val NumDecks = 4

    /** Checks if the proposed number of human players is within the allowed blackjack limits.
     *
     * @param num The number of human players to validate.
     * @return [[true]] if the number is between [[MinPlayersNum]] and [[MaxPlayersNum]] (inclusive), [[false]] otherwise.
     */
    def isPlayerNumValid(num: Int): Boolean = num >= MinPlayersNum && num <= MaxPlayersNum

    /** Validates the initial deposit amount for a player.
     *
     * A deposit is considered valid if it is a positive amount and a multiple
     * of the smallest available fiche denomination.
     *
     * @param amount The deposit amount to validate.
     * @return [[true]] if the deposit is positive and properly aligned with fiche denominations, [[false]] otherwise.
     */
    def isInitialDepositValid(amount: Double): Boolean =
      amount > 0 && amount % Fiche.smallestDenomination == 0 && amount >= MinGameBet

    /** Validates a list of human player names based on the game's registration constraints.
     *
     * The rules require that:
     * - The number of names matches the expected count of human players.
     * - There are no duplicate names.
     * - No name is empty or contains the reserved character "_".
     *
     * @param expectedCount The exact number of human players expected.
     * @param names         The list of names entered by the user.
     * @return [[true]] if all conditions are met, [[false]] if the list contains duplicates,
     *         invalid characters, or does not match the expected count.
     */
    def arePlayersNamesValid(expectedCount: Int)(names: List[String]): Boolean =
      names.length == expectedCount &&
        names.distinct.length == names.length &&
        !names.exists(name => name.isEmpty || name.contains("_"))

    def apply(players: List[Player]): Game =
      val totalParticipants = MaxPlayersNum + 1
      GameImpl(players, List.empty, Deck.generateDeck(NumDecks, totalParticipants).shuffle(totalParticipants))

    def apply(players: List[Player], deck: Deck): Game = GameImpl(players, List.empty, deck)

    private class GameImpl(private var currentPlayers: List[Player],
                           override var currentBets: List[Bet],
                           private var currentDeck: Deck) extends Game:

      private val BlackjackPayoutMultiplier = 2.5
      private val gameDealer: Dealer = Dealer()
      private var cutCardInDeck: Boolean = true
      private var cutCardMessageShown: Boolean = false

      override def players: List[Player] = currentPlayers

      override def balances(players: List[Player]): List[(String, Double)] =
        players.map(p => (p.name, p.balance.totalValue))

      override def dealer: Dealer = gameDealer

      override def deck: Deck = currentDeck

      override def addBots(): Unit =
        val botsNeeded: Int = MaxPlayersNum - currentPlayers.size
        val bots = (1 to botsNeeded).map(i => BotPlayer(s"Bot$i")).toList
        currentPlayers = currentPlayers ::: bots
      
      override def isNameValid(name: String): Boolean = players.exists(_.name == name)

      override def isBetValid(player: Player)(amount: Double): Boolean =
        amount > 0 && amount % MinGameBet == 0 && amount <= player.balance.totalValue

      override def drawStandardCard(): Option[StandardCard] =
        val (optCard, newDeck) = deck.draw()
        currentDeck = newDeck
        optCard match
          case Some(card: StandardCard) => Some(card)
          case Some(CutCard)            =>
            cutCardInDeck = false
            drawStandardCard()
          case _                        => None

      override def drawCard(participant: Participant): Option[StandardCard] =
        drawStandardCard().map: card =>
          participant.addCard(card)
          card

      override def distributeCards(): List[String] =
        def distributeCards_(participants: List[Participant], faceUp: Boolean = true): List[String] =
          participants.flatMap(participant =>
            drawStandardCard() match
              case Some(card: StandardCard) =>
                if participant.isInstanceOf[Dealer] && !faceUp then
                  participant.addCard(card.flip())
                else
                 participant.addCard(card)
                List(participant.toString)
              case _                        => List.empty
          )
        val participants: List[Participant] = players :+ gameDealer
        val firstRound = distributeCards_(participants)
        val secondRound = distributeCards_(participants, faceUp = false)
        firstRound ::: secondRound

      override def dealerHasAce: Boolean = dealer.cards.exists(card => card.value == Value.Ace && card.isFaceUp)
        
      override def initialBlackjackPlayers(): List[Player] =
        currentPlayers.filter(player => player.cards.isBlackjack)

      override def playersWithBlackjack(players: List[Player]): List[Player] =
        players.filter(_.cards.isBlackjack)

      override def handleBlackjacks(winners: List[Player]): Unit =
        def dealerMightHaveBlackjack: Boolean =
          Set(Value.Ace, Value.Ten, Value.Jack, Value.Queen, Value.King).contains(gameDealer.cards.head.value)

        winners.foreach(playerWithBJ =>
          playerWithBJ.winBlackjack()
          if !dealerMightHaveBlackjack then
            val bet = currentBets.find(_.player == playerWithBJ).get
            playerWithBJ.deposit(bet.amount * BlackjackPayoutMultiplier)
            currentBets = currentBets.filterNot(_.player == playerWithBJ)
        )

      override def evaluatePlayerBust(player: Player): Boolean =
        val busted = player.cards.isBusted
        if busted then player.bust()
        busted

      override def handleInsurances(names: List[String]): Unit =
        val insurancePercentage = 0.5
        val insuredPlayers: List[NormalPlayer] = 
          currentPlayers.collect:
            case p:NormalPlayer if names.contains(p.name) => p
        insuredPlayers.foreach(p =>
          currentBets.find(_.player == p).foreach(prevBet =>
            val insuranceBet = prevBet.amount * insurancePercentage
            if p.balance.totalValue >= insuranceBet then
              p.hasInsurance = true
              p.withdraw(insuranceBet)
              val newBet = Bet(p, (prevBet.amount + insuranceBet).toInt)
              currentBets = currentBets.map(bet =>
                if bet.player == p then newBet else bet)
          )
        )

      override def resolveInsurances(): List[(String, Double)] =
        def resolveBet(bet: Bet): (Bet, Option[(String, Double)]) = bet.player match
          case player: NormalPlayer if player.hasInsurance =>
            val insuranceAmount = bet.amount / 3
            val restoredBet = Bet(bet.player, bet.amount - insuranceAmount)
            val win = gameDealer.cards.isBlackjack match
              case true =>
                val payout = insuranceAmount * 2.0
                bet.player.deposit(payout)
                Some(bet.player.name, payout)
              case _    => None
            (restoredBet, win)
          case _                                            => (bet, None)
        val (updatedBets, results) = currentBets.map(resolveBet).unzip
        currentBets = updatedBets
        results.filter(_.isDefined).map(_.get)

      override def canDoubleDown(player: Player): Boolean =
        val playerBet = currentBets.find(_.player == player).map(_.amount).getOrElse(0)
        player.cards.size == 2 && player.balance.totalValue >= playerBet

      override def doubleDown(player: Player): Option[Card] =
        val bet = currentBets.find(_.player == player).get
        player.withdraw(bet.amount)
        currentBets = currentBets.map(b => if b.player == player then b.copy(amount = b.amount * 2) else b)
        drawCard(player)

      protected def countSplits(player: Player): Int =
        currentBets.count(bet => bet.player.name.contains(player.name + "_split"))

      override def canSplit(player: Player): Boolean =
        def isAce(card: StandardCard): Boolean = card.value == Value.Ace
        val bet = currentBets.find(_.player == player).map(_.amount.toDouble).getOrElse(0.0)
        def baseSplitRule(card1: StandardCard, card2: StandardCard): Boolean =
          card1.value == card2.value && player.balance.totalValue >= bet
        player.cards match
          case List(first, second) if isAce(first)=>
            !player.isInstanceOf[SplitPlayer] && countSplits(player) == 0 && baseSplitRule(first, second)
          case List(first, second) =>
            baseSplitRule(first, second)
          case _ =>
            false

      override def splitPlayer(player: Player): Option[(Card, Card)] =
        @tailrec
        def addPlayerAfter(targetPlayer: Player,
                           addedPlayer: SplitPlayer,
                           players: List[Player],
                           acc: List[Player]): List[Player] =
          players match
            case h :: t if h == targetPlayer =>
              acc ::: List(h, addedPlayer) ::: t
            case h :: t =>
              addPlayerAfter(targetPlayer, addedPlayer, t, acc :+ h)
            case _ => acc

        def handleSplitPlayerBet(splitPlayer: SplitPlayer): Unit =
          val initialBetPercentage: Double = 2.0 / 3.0
          val playerBet = currentBets.find(_.player == player).get.amount
          val actualPlayerBet = player match
            case player: NormalPlayer if player.hasInsurance => (playerBet * initialBetPercentage).toInt
            case _ => playerBet
          player.withdraw(actualPlayerBet)
          currentBets = currentBets :+ Bet(splitPlayer, actualPlayerBet)

        def handleCardsDistribution(first: StandardCard, splitPlayer: SplitPlayer): Option[(Card, Card)] =
          player.clearHand()
          player.addCard(first)
          val firstDraw = drawCard(player)
          val secondDraw = drawCard(splitPlayer)
          for
            card1 <- firstDraw
            card2 <- secondDraw
          yield (card1, card2)

        val List(first, second) = player.cards
        val splitPlayerName = player.name + "_split" + (countSplits(player) + 1).toString
        val splitPlayer = SplitPlayer(splitPlayerName, second)
        currentPlayers = addPlayerAfter(player, splitPlayer, currentPlayers, List.empty)
        handleSplitPlayerBet(splitPlayer)
        handleCardsDistribution(first, splitPlayer)

      override def getNextPlayer(targetPlayer: Player): Option[Player] =
        val index = currentPlayers.indexOf(targetPlayer)
        if index != -1 && index < currentPlayers.length - 1 then
          Some(currentPlayers(index + 1))
        else
          None

      override def transferBalance(player: Player): Unit =
        val name = if player.isInstanceOf[SplitPlayer] then player.name.split("_").head else player.name
        val index = currentPlayers.indexOf(player)
        val balance = player.balance.totalValue
        if index < currentPlayers.length - 1 then
          val nextPlayer = currentPlayers(index + 1)
          if nextPlayer.name.contains(name) then
            player.withdraw(balance)
            nextPlayer.deposit(balance)

      override def evaluateDealerBust: Boolean = dealer.cards.isBusted

      override def computeBotTurn(bot: BotPlayer): List[String] = computeAutomaticTurn(bot, bot.name, List.empty[String], () => bot.hasFinishedTurn)

      override def computeDealerTurn(): List[String] =
        dealer.revealCards()
        computeAutomaticTurn(dealer, "the dealer", List(dealer.toString), () => dealer.hasFinishedTurn)

      override def handlePayout(): Unit =
        val dealerBusted = evaluateDealerBust
        val dealerBJ = gameDealer.cards.isBlackjack
        val dealerScore = gameDealer.score.playableValue

        def creditPlayer(player: Player, payout: Double): Unit = player match
          case sp: SplitPlayer =>
            val originalPlayer = currentPlayers.find(_.name == player.name.split("_").head).get
            originalPlayer.deposit(player.balance.totalValue + payout)
          case _               =>
            if payout > 0 then player.deposit(payout)

        def resolve(player: Player): Unit =
          val bet = currentBets.find(_.player == player).get.amount
          val playerBJ = player.state == PlayerState.Blackjack
          val playerBusted = player.state == PlayerState.Busted
          val playerScore = player.score.playableValue
          val payout: Double = (playerBusted, dealerBusted, playerBJ, dealerBJ) match
            case (true, _, _, _) | (_, _, false, true) => 0
            case (_, _, true, true)                    => bet
            case (_, _, true, false)                   => bet * BlackjackPayoutMultiplier
            case (_, true, _, _)                       => bet * 2
            case _ if playerScore > dealerScore        => bet * 2
            case _ if playerScore == dealerScore       => bet
            case _                                     => 0
          creditPlayer(player, payout)
          gameDealer.addProfit(bet - payout)

        currentPlayers.filter(p => currentBets.exists(_.player == p)).foreach(resolve)

      override def brokePlayers(): List[Player] = currentPlayers.filter(_.balance.totalValue < MinGameBet)

      override def removeBrokePlayers(): Unit = removePlayers(brokePlayers().contains(_))

      override def removeSplitPlayers(): Unit = removePlayers(_.isInstanceOf[SplitPlayer])

      override def removeLeavingPlayers(leavingPlayers: List[Player]): Unit = removePlayers(leavingPlayers.contains(_))

      override def handleHandEnd(): Unit =
        currentPlayers.foreach(_.prepareForNewHand())
        gameDealer.clearHand()

      override def isCutCardInDeck: Boolean = cutCardInDeck

      override def shouldShowCutCardMessage: Boolean =
        if !cutCardInDeck && !cutCardMessageShown then
          cutCardMessageShown = true
          true
        else
          false

      override def areAllHumanPlayersOut: Boolean =
        currentPlayers.collectFirst:
          case _: NormalPlayer => true
        .isEmpty

      override def isOver: Boolean = !isCutCardInDeck || areAllHumanPlayersOut

      /** Continues to draw cards for a participant until their
       * termination condition is met, accumulating status messages for each step.
       *
       * @param participant     The participant drawing cards (must match the type expected by `drawCard`).
       * @param displayName     The name to display in the generated messages (e.g., bot name or "the dealer").
       * @param initialMessages The starting list of messages, usually containing the participant's initial state.
       * @param hasFinished     A higher-order function evaluated at each iteration to determine if the participant must stop drawing.
       * @return A [[List]] of [[String]] containing the history of all state transitions and dealt cards.
       */
      private def computeAutomaticTurn(
          participant: Participant,
          displayName: String,
          initialMessages: List[String],
          hasFinished: () => Boolean
      ): List[String] =
        @tailrec
        def extractUntilSeventeen(acc: List[String]): List[String] =
          if hasFinished() then acc
          else
            drawCard(participant) match
              case Some(card) =>
                val msg = s"A new card will be dealt to $displayName:\n$card\n$participant"
                extractUntilSeventeen(acc :+ msg)
              case _          => List.empty
        extractUntilSeventeen(initialMessages)

      /** Removes from the game all players that satisfy a given condition.
       *
       * @param isToRemove whether a player is to remove or not.
       */
      private def removePlayers(isToRemove: Player => Boolean): Unit =
        currentPlayers = currentPlayers.filterNot(isToRemove)




