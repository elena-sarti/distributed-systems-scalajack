package view

import protocol.{Decision, PlayerView, TableView}

/** The console of a player at a table on the network: the same prompts and messages as the local game ([[CLIView]])
 * where the two games ask the same things, plus what only the online game needs (account, leaving the table). */
object OnlineView:
  import utils.GameUIExports.*
  import utils.ModelExports.{StandardCard, Suit, Value, calculateScore}
  import CLIView.promptUntilValid

  enum Account:
    case Login, Register

  final case class Credentials(account: Account, username: String, password: String)

  enum BetChoice:
    case Bet(amount: Int)
    case Leave

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

  /** The bet for the next hand, or the wish to leave the table. The table checks the bet again. */
  def askBet(balance: Double, minBet: Int)(using console: Console[IO]): IO[BetChoice] =
    promptUntilValid(
      prompt = s"Your balance is $balance fiches. Type your bet for the next hand (a multiple of $minBet), " +
        "or \"L\" to leave the table.",
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

  def askInsurance(using console: Console[IO]): IO[Boolean] =
    yesOrNo("The dealer shows an Ace! Do you want to buy the insurance, for half of your bet? (Y/N)")

  def askToPlayAgain(using console: Console[IO]): IO[Boolean] = yesOrNo("Do you want to sit at a table again? (Y/N)")

  /** The table as a player sees it: the dealer, then every player with cards, score, bet and balance. */
  def describeTable(table: TableView, me: String): String =
    val dealer  = s"[Dealer]\n${sideBySide(table.dealer.visibleCards ++ List.fill(table.dealer.hiddenCards)(Hidden))}" +
      s"\nSCORE: ${table.dealer.visibleCards.calculateScore}"
    val players = table.players.map: player =>
      val who  = if player.name == me then s"${player.name} (you)" else player.name
      val turn = if table.currentTurn.contains(player.name) then "  <- turn" else ""
      s"[$who]$turn\n${sideBySide(player.cards)}\nSCORE: ${player.cards.calculateScore}   STATE: ${player.state}   " +
        s"BET: ${player.bet.getOrElse("-")}   BALANCE: ${player.balance}"
    (s"----------------------------------------" :: dealer :: players).mkString("\n\n") + "\n"

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
    case Decision.Action    => "Time is up: your hand stands.\n")

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
