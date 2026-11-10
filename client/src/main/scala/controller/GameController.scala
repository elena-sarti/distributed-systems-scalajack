package controller

import utils.GameUIExports.*

enum TurnOutcome:
  case Continue
  case Stop

object GameController extends IOApp.Simple:
  import utils.ModelExports.*

  /** The main entry point of the application.
   * Initializes the game state, orchestrates the loop of hands until
   * the game is over, and handles the final game-over screens.
   */
  def run: IO[Unit] =
    for
      game <- initializeGame
      _    <- handleHands(game)
      _    <- endGame(game)
    yield ()

  /** Initializes a new game instance by prompting the user for the number of players,
   * gathering their names, and collecting their initial deposits.
   *
   * @return An [[IO]] effect containing the initialized [[Game]] instance.
   */
  def initializeGame(using console: Console[IO]): IO[Game] =
    for
      numPlayers <- getNumPlayers(Game.isPlayerNumValid)
      players    <- getPlayers(numPlayers)
      game       = Game(players)
      _          <- IO(game.addBots())
    yield game

  /** Execution loop that repeatedly plays hands until the game's termination condition is met.
   *
   * @param game The current game instance.
   */
  def handleHands(game: Game)(using console: Console[IO]): IO[Unit] =
    handleHand(game).iterateUntil(_ => game.isOver)

  /** Handles the actions at the end of a game, rendering the gameover message
   * and displaying the final balances of all players still at the table.
   *
   * @param game The current game instance.
   */
  def endGame(game: Game)(using console: Console[IO]): IO[Unit] =
    renderMessage(GameOver) >>
      IO.whenA(game.players.nonEmpty):
        game.balances(game.players).traverse_(nameAndBalance => renderMessage(ShowFinalBalance(nameAndBalance._1, nameAndBalance._2)))

  /** Prompts for names and initial deposits to register a list of human players.
   *
   * @param numPlayers The number of players to register.
   * @return An [[IO]] containing a list of registered [[Player]] instances.
   */
  def getPlayers(numPlayers: Int)(using console: Console[IO]): IO[List[Player]] =
    for
      playersNames <- getPlayersNames(Game.arePlayersNamesValid(numPlayers))
      players      <- playersNames.traverse(name =>
          getInitialDeposit(name, Game.isInitialDepositValid, Game.MinGameBet).map(balance => NormalPlayer(name, balance))
      )
    yield players

  /** Collects bets from all active players, withdraws the amounts from their bankrolls,
   * and updates the current betting state in the game model.
   *
   * @param game The current game instance.
   */
  def getBets(game: Game)(using console: Console[IO]): IO[Unit] =
    IO(game.players).flatMap: players =>
      players.traverse:
        case bot: BotPlayer => IO.pure(Bet(bot, bot.computeSafeBet))
        case human          => getBet(human, game.isBetValid(human), Game.MinGameBet).map(Bet(human, _))
    .flatMap(bets => IO:
      bets.foreach(bet => bet.player.withdraw(bet.amount))
      game.currentBets = bets
    )

  /** Prepares a new hand by collecting bets, distributing initial cards,
   * checking for Blackjacks, and handling optional insurance logic.
   *
   * @param game The current game instance.
   */
  def initializeHand(game: Game)(using console: Console[IO]): IO[Unit] =
    getBets(game) >>
      renderMessage(CardsDistribution) >>
      game.distributeCards().traverse_(card => renderMessage(ShowCard(card))) >>
      IO.whenA(game.shouldShowCutCardMessage)(renderMessage(ShowCutCard)) >>
      handleBlackjacksWinners(game) >>
      IO.whenA(game.dealerHasAce):
        getInsurancePlayers(game.isNameValid).flatMap(insuranceNames => IO(game.handleInsurances(insuranceNames)))

  /** Identifies and processes players who scored a Blackjack on the initial deal.
   * Supports evaluation of split hands if provided.
   *
   * @param game                  The current game instance.
   * @param splitBlackjackPlayers Optional subset of players representing split hands to evaluate.
   */
  def handleBlackjacksWinners(game: Game, splitBlackjackPlayers: List[Player] = List.empty)(using console: Console[IO]): IO[Unit] =
    val winners: List[Player] = if splitBlackjackPlayers.isEmpty then game.initialBlackjackPlayers() else splitBlackjackPlayers
    IO(game.handleBlackjacks(winners)) >>
      winners.traverse_(winner => renderMessage(ShowBlackJack(winner)))

  /** Iterates through all active players to execute their individual turns sequentially,
   * skipping those who already have a Blackjack.
   *
   * @param game The current game instance.
   */
  def handlePlayersTurn(game: Game)(using console: Console[IO]): IO[Unit] =
    def loop(playerOpt: Option[Player]): IO[Unit] = playerOpt match
      case Some(player) =>
        for
          _          <- IO.unlessA(player.state == Blackjack)(handleSinglePlayerTurn(game, player))
          nextPlayer <- IO(game.getNextPlayer(player))
          _          <- loop(nextPlayer)
        yield ()
      case _           => IO.unit
    IO(game.players.headOption).flatMap(loop)

  /** Routes and processes a single turn decision (Hit, Double, Stand, Split) made by a player.
   *
   * @param game   The current game instance.
   * @param player The player performing the action.
   * @param action The specific [[PlayerAction]] selected.
   * @return       An [[IO]] containing [[Continue]] if the player can continue their turn, [[Stop]] otherwise.
   */
  def handlePlayerAction(game: Game, player: Player, action: PlayerAction)(using console: Console[IO]): IO[TurnOutcome] = action match
    case PlayerAction.DrawCard   => drawAndProcess(game, player, game.drawCard, autoStop = false)
    case PlayerAction.DoubleDown => drawAndProcess(game, player, game.doubleDown, autoStop = true)
    case PlayerAction.Stand      => finalizePlayerTurn(game, player)
    case PlayerAction.Split      => processSplit(game, player)

  /** Orchestrates the dealer's automated turn sequence: reveals the hidden card,
   * resolves active insurances, and draws cards according to the rules.
   *
   * @param game The current game instance.
   */
  def handleDealerTurn(game: Game)(using console: Console[IO]): IO[Unit] =
    renderMessage(DealerTurn()) >>
      renderMessage(ShowCard(game.dealer.toString)) >>
      game.resolveInsurances().traverse_((name, win) => renderMessage(ShowInsuranceWin(name, win))) >>
      game.computeDealerTurn().traverse_(card => processCardDrawing(game, card))

  /** Compares player hands against the dealer, determines payouts,
   * updates bankrolls, and renders individual round results.
   *
   * @param game The current game instance.
   */
  def handleHandWinners(game: Game)(using console: Console[IO]): IO[Unit] =
    IO.whenA(game.evaluateDealerBust)(renderMessage(DealerBusted)) >>
      IO(game.handlePayout()) >>
      renderMessage(HandOver) >>
      game.balances(game.players).traverse_(nameAndBalance => renderMessage(ShowBalance(nameAndBalance._1, nameAndBalance._2)))

  /** Concludes the current round of play by cleaning up split hands,
   * removing bankrupt players, handling voluntary departures, and resetting hand states.
   *
   * @param game The current game instance.
   */
  def finalizeHand(game: Game)(using console: Console[IO]): IO[Unit] =
    IO(game.removeSplitPlayers()) >>
      handleBrokePlayers(game) >>
      handleLeavingPlayers(game) >>
      IO(game.handleHandEnd())

  /** Runs the sequential phases comprising a complete round of Blackjack. */
  private def handleHand(game: Game)(using console: Console[IO]): IO[Unit] =
    initializeHand(game) >>
      handlePlayersTurn(game) >>
      handleDealerTurn(game) >>
      handleHandWinners(game) >>
      finalizeHand(game)

  /** Manages the complete interactive lifecycle of a single player's turn.
   * Dispatches execution flow between automated bot behavior and human choice loops.
   *
   * @param player The active player.
   */
  private def handleSinglePlayerTurn(game: Game, player: Player)(using console: Console[IO]): IO[Unit] =
    def _handleSinglePlayerTurn(player: Player): IO[Unit] =
      getPlayerAction(player, game.canDoubleDown, game.canSplit).flatMap(action =>
        handlePlayerAction(game, player, action).flatMap:
          case TurnOutcome.Continue => _handleSinglePlayerTurn(player)
          case _                    => IO.unit
      )
    renderMessage(PlayerTurn(player.name)) >>
      renderMessage(ShowCard(player.toString)) >> (
      player match
        case bot: BotPlayer => game.computeBotTurn(bot).traverse_(card => processCardDrawing(game, card))
        case _              => _handleSinglePlayerTurn(player)
      )

  /** Transitions a player into the stand state and updates their financial balances.
   *
   * @return An [[IO]] containing [[Stop]] to forcefully break any decision loops.
   */
  private def finalizePlayerTurn(game: Game, player: Player): IO[TurnOutcome] =
    IO(player.stand()) >>
      IO(game.transferBalance(player)) >>
      IO.pure(TurnOutcome.Stop)

  /** Assesses a player's hand status post card-drawing. Handles busts,
   * enforces auto-stand boundaries (e.g. hitting exactly 21), or allows additional choices.
   *
   * @param autoStop Whether the player must automatically stop drawing cards.
   * @return         An [[IO]] containing [[Continue]] if the player may keep drawing, [[Stop]] if their turn ends.
   */
  private def processPostDrawState(game: Game, player: Player, autoStop: Boolean): IO[TurnOutcome] =
    IO(game.evaluatePlayerBust(player)).flatMap:
      case true                                                        =>
        renderMessage(ShowBusted(player)) >>
          IO(game.transferBalance(player)) >>
          IO.pure(TurnOutcome.Stop)
      case _ if autoStop || player.score.playableValue == WinningScore => finalizePlayerTurn(game, player)
      case _                                                           => IO.pure(TurnOutcome.Continue)

  /** Executes a card extraction effect and hands off validation to the post-draw state processor.
   *
   * @param drawEffect The injection logic performing the card allocation.
   * @param autoStop   Whether the player must automatically stop drawing cards.
   * @return           An [[IO]] containing [[Continue]] if the player may keep drawing, [[Stop]] if their turn ends.
   */
  private def drawAndProcess(game: Game, player: Player, drawEffect: Player => Option[Card], autoStop: Boolean)(using console: Console[IO]): IO[TurnOutcome] = drawEffect(player) match
    case Some(card) =>
      processCardDrawing(game, s"$card\n$player") >>
        processPostDrawState(game, player, autoStop)
    case _          => IO.pure(TurnOutcome.Stop)

  /** Processes pairs-splitting actions, handles nested blackjacks on split configurations,
   * and adjusts game state tracking for the newly spawned hands.
   *
   * @return An [[IO]] containing [[Continue]] if the player may keep drawing, [[Stop]] if their turn ends.
   */
  private def processSplit(game: Game, player: Player)(using console: Console[IO]): IO[TurnOutcome] = game.splitPlayer(player) match
    case Some(cardPlayer, _) =>
      val splitPlayers = List(player, game.getNextPlayer(player).get)
      val blackjackPlayers = game.playersWithBlackjack(splitPlayers)
      renderMessage(ShowCard(s"$cardPlayer\n$player")) >>
        IO.whenA(blackjackPlayers.nonEmpty)(handleBlackjacksWinners(game, splitBlackjackPlayers = blackjackPlayers)) >>
        IO.pure(if blackjackPlayers.contains(player) then TurnOutcome.Stop else TurnOutcome.Continue)
    case _                   => IO.pure(TurnOutcome.Stop)

  /** Outputs drawn card information to the UI view and checks whether the deck's cut-card has been reached. */
  private def processCardDrawing(game: Game, cardMessage: String)(using console: Console[IO]): IO[Unit] =
    IO.whenA(game.shouldShowCutCardMessage)(renderMessage(ShowCutCard)) >>
      renderMessage(ShowCard(cardMessage))

  /** Helper method to report final balances and clear a specific list of players from the game. */
  private def reportAndRemovePlayers(game: Game, players: List[Player])(removeEffect: IO[Unit])
                                    (using console: Console[IO]): IO[Unit] =
    for
      balances <- IO(game.balances(players))
      _        <- balances.traverse_(nameAndBalance => renderMessage(ShowFinalBalance(nameAndBalance._1, nameAndBalance._2)))
      _        <- removeEffect
    yield ()

  /** Handles the removal of broke players. */
  private def handleBrokePlayers(game: Game)(using console: Console[IO]): IO[Unit] =
    for
      brokePlayers <- IO(game.brokePlayers())
      _            <- reportAndRemovePlayers(game, brokePlayers)(IO(game.removeBrokePlayers()))
    yield ()

  /** Prompts for any voluntary table exits, settles final bankrolls for leaving accounts,
   * and triggers their teardown from active memory.
   */
  private def handleLeavingPlayers(game: Game)(using console: Console[IO]): IO[Unit] =
    IO.unlessA(game.areAllHumanPlayersOut):
      for
        leavingNames   <- getLeavingPlayers(game.isNameValid)
        leavingPlayers <- IO(game.players.filter(p => leavingNames.contains(p.name)))
        _              <- reportAndRemovePlayers(game, leavingPlayers)(IO(game.removeLeavingPlayers(leavingPlayers)))
      yield ()
