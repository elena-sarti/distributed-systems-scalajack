package view

import protocol.{Decision, PlayerView, TableView}

/** The console of a player at a table on the network: the same prompts and messages as the local game ([[CLIView]])
 * where the two games ask the same things, plus what only the online game needs (account, leaving the table). */
object OnlineView:
  import utils.GameUIExports.*
  import utils.ModelExports.{PlayerState, Score, StandardCard, Suit, Value, calculateScore}
  import CLIView.promptUntilValid

  enum Account:
    case Login, Register

  final case class Credentials(account: Account, username: String, password: String)

  enum BetChoice:
    case Bet(amount: Int)
    case Leave

  /** What a player can do in the lobby, where they are before sitting at a table and when they have left one. */
  enum LobbyChoice:
    case Play, Deposit, Withdraw, Quit

  def welcome(lobbyUrl: String)(using console: Console[IO]): IO[Unit] =
    console.println(s"Welcome to ScalaJack online! The lobby is at $lobbyUrl.\n")

  def askCredentials(using console: Console[IO]): IO[Credentials] =
    for
      account <- promptUntilValid(
        prompt = "Type \"L\" to log in, or \"R\" to register a new account.",
        parser = _.toUpperCase match
          case "L" => Some(Account.Login)
          case "R" => Some(Account.Register)
          case _   => None,
        predicate = _ => true,
        successMessage = _ => "",
        errorMessage = "Sorry, type \"L\" or \"R\".")
      username <- promptUntilValid[String]("Username:", Some(_), _.nonEmpty, _ => "", "The username cannot be empty.")
      password <- promptUntilValid[String]("Password:", Some(_), _.nonEmpty, _ => "", "The password cannot be empty.")
    yield Credentials(account, username, password)

  /** The menu of the lobby, under the balance of the player (`None` if the lobby could not tell it). */
  def askLobbyChoice(balance: Option[Double])(using console: Console[IO]): IO[LobbyChoice] =
    promptUntilValid(
      prompt = s"${balance.fold("Your balance is not available now.")(money => s"Your balance is $money fiches.")}\n" +
        "Type \"P\" to sit at a table, \"D\" to make a deposit, \"W\" to withdraw, or \"Q\" to quit.",
      parser = _.toUpperCase match
        case "P" => Some(LobbyChoice.Play)
        case "D" => Some(LobbyChoice.Deposit)
        case "W" => Some(LobbyChoice.Withdraw)
        case "Q" => Some(LobbyChoice.Quit)
        case _   => None,
      predicate = _ => true,
      successMessage = _ => "",
      errorMessage = "Sorry, type \"P\", \"D\", \"W\" or \"Q\".")

  /** The amount of fiches to deposit or to withdraw (`verb` says which), or `None` if the player changes their mind.
   * The lobby checks the amount again: here only what cannot be an amount is refused. */
  def askAmount(verb: String)(using console: Console[IO]): IO[Option[Double]] =
    promptUntilValid[Option[Double]](
      prompt = s"How many fiches do you want to $verb? (or \"C\" to cancel)",
      parser = input => if input.equalsIgnoreCase("C") then Some(None) else input.toDoubleOption.map(Some(_)),
      predicate = _.forall(amount => amount.isFinite && amount > 0),
      successMessage = _ => "",
      errorMessage = "Sorry, type a positive number of fiches, or \"C\".")

  /** The bet for the next hand, or the wish to leave the table. The table checks the bet again. */
  def askBet(balance: Double, minBet: Int, footer: String = "")(using console: Console[IO]): IO[BetChoice] =
    promptUntilValid(
      prompt = s"Your balance is $balance fiches. Type your bet for the next hand (a multiple of $minBet), " +
        s"or \"L\" to leave the table.$footer",
      parser = input =>
        if input.equalsIgnoreCase("L") then Some(BetChoice.Leave) else input.toIntOption.map(BetChoice.Bet(_)),
      predicate =
        case BetChoice.Bet(amount) => amount >= minBet && amount % minBet == 0 && amount <= balance
        case BetChoice.Leave       => true,
      successMessage =
        case BetChoice.Bet(amount) => s"Your bet of $amount fiches is on the table, waiting for the others...\n"
        case BetChoice.Leave       => "You leave the table.\n",
      errorMessage = s"Sorry, your bet must be a multiple of $minBet fiches (and at least $minBet), " +
        s"and it cannot exceed your balance ($balance fiches).")

  def askInsurance(footer: String = "")(using console: Console[IO]): IO[Boolean] =
    yesOrNo(s"The dealer shows an Ace! Do you want to buy the insurance, for half of your bet? (Y/N)$footer")

  /** The line with the time left to answer, shown under a question. */
  def timeLeft(seconds: Long): String = s"Time left: $seconds s"

  /** Writes the time left again on the line right above the cursor (the one written by [[timeLeft]]), leaving the
   * cursor where it is: the player may be typing there. Uses the ANSI sequences of the terminals: save the cursor,
   * go one line up, rewrite the line, go back. */
  def refreshTimeLeft(seconds: Long)(using console: Console[IO]): IO[Unit] =
    console.print(s"\u001b7\u001b[1A\r${timeLeft(seconds)}\u001b[K\u001b8")

  /** Short forms of the questions, repeated under the table when it changes while the player has not answered yet. */
  def betReminder(minBet: Int): String = s"Waiting for your bet (a multiple of $minBet), or \"L\" to leave the table."
  val insuranceReminder: String = "Waiting for your answer: do you want the insurance? (Y/N)"
  def actionReminder(canDoubleDown: Boolean, canSplit: Boolean): String =
    val options = List("\"D\" draw", "\"S\" stand") ++ Option.when(canDoubleDown)("\"O\" double down") ++
      Option.when(canSplit)("\"P\" split")
    s"Waiting for your action: ${options.mkString(", ")}."

  /** The table as a player sees it: the dealer, then every player with cards, score, bet and balance. */
  def describeTable(table: TableView, me: String): String =
    val dealerScore = table.dealer.visibleCards.calculateScore
    val dealerDone  = table.dealer.hiddenCards == 0 && dealerScore.playableValue >= DealerStandsFrom
    val dealer  = s"[Dealer]\n${sideBySide(table.dealer.visibleCards ++ List.fill(table.dealer.hiddenCards)(Hidden))}" +
      s"\nSCORE: ${shownScore(dealerScore, finished = dealerDone)}"
    val players = table.players.map: player =>
      val who  = if player.name == me then s"${player.name} (you)" else player.name
      val turn = if table.currentTurn.contains(player.name) then "  <- turn" else ""
      val score = shownScore(player.cards.calculateScore, finished = player.state != PlayerState.Active)
      s"[$who]$turn\n${sideBySide(player.cards)}\nSCORE: $score   STATE: ${player.state}   " +
        s"BET: ${player.bet.getOrElse("-")}   BALANCE: ${player.balance}"
    (s"----------------------------------------" :: dealer :: players).mkString("\n\n") + "\n"

  /** The dealer stops drawing from this value on, as in the local game. */
  private val DealerStandsFrom = 17

  /** Both readings of a hand with an ace ("7 / 17") while there is still a choice to make; once the hand is played
   * only its best value counts, as the local game does for the dealer. */
  private def shownScore(score: Score, finished: Boolean): String =
    if finished then score.playableValue.toString else score.toString

  def renderTable(table: TableView, me: String)(using console: Console[IO]): IO[Unit] =
    console.println(describeTable(table, me))

  /** The balances at the end of a hand, as in the local game. */
  def renderResults(balances: List[(String, Double)])(using console: Console[IO]): IO[Unit] =
    renderMessage(HandOver) >> balances.traverse_((name, balance) => renderMessage(ShowBalance(name, balance)))

  /** The end of the game at this table (the cut card came out), as in the local game. */
  def renderGameOver(balances: List[(String, Double)])(using console: Console[IO]): IO[Unit] =
    renderMessage(GameOver) >> balances.traverse_((name, balance) => renderMessage(ShowFinalBalance(name, balance)))

  def renderTimeUp(decision: Decision)(using console: Console[IO]): IO[Unit] = console.println(decision match
    case Decision.Bet       => "Time is up: you sit out this hand.\n"
    case Decision.Insurance => "Time is up: no insurance for you.\n"
    case Decision.Action    => "Time is up: the table plays your hand (a card while it is worth less than 12, then it stands).\n")

  def info(text: String)(using console: Console[IO]): IO[Unit] = console.println(text)

  private val Hidden: StandardCard = StandardCard(Suit.Spades, Value.Ace, isFaceUp = false) // shown as the back of a card

  /** The cards next to each other, as the local game shows them. */
  private def sideBySide(cards: List[StandardCard]): String =
    if cards.isEmpty then "(no cards)"
    else
      val lines = cards.map(_.toString.linesIterator.toVector)
      lines.head.indices.map(row => lines.map(_(row)).mkString("  ")).mkString("\n")

  private def yesOrNo(question: String)(using console: Console[IO]): IO[Boolean] =
    promptUntilValid(
      prompt = question,
      parser = _.toUpperCase match
        case "Y" => Some(true)
        case "N" => Some(false)
        case _   => None,
      predicate = _ => true,
      successMessage = _ => "",
      errorMessage = "Sorry, type \"Y\" or \"N\".")
