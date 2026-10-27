package server

import protocol.{ClientMessage, ServerMessage, TableView}
import protocol.ClientMessage.*
import protocol.ServerMessage.*
import utils.ModelExports.*

/** The logic of one table: who is seated and how a hand is played through the clients' messages.
 *
 * It is the only authority on the game state: clients send requests, the table validates them,
 * updates the [[Game]] and tells every seated client the new state.
 * The flow of a hand is a state machine (see [[TableHandler.Phase]]) moved forward by messages.
 * NOT thread-safe on purpose: it must be driven by a [[SingleWriterHandler]], so that it is only
 * ever called by one thread and can use plain mutable state.
 *
 * @param initialBalance the balance given to a player when sitting down.
 * @param newGame        creates the game of the table (tests pass a game with a known deck).
 */
final class TableHandler(initialBalance: Double = TableHandler.DefaultBalance,
                         newGame: () => Game = () => Game(List.empty)) extends SessionHandler:
  import TableHandler.*

  private var game = newGame()

  /** The seated players, by connection id. */
  private var seats = Map.empty[String, Seat]

  /** Where the current hand is. */
  private var phase: Phase = Phase.Idle

  def onConnected(connection: Connection): Unit = () // nothing to do until the client asks to join

  def onMessage(connection: Connection, message: ClientMessage): Unit =
    seats.get(connection.id) match
      case None => message match
        case JoinTable(name) => join(connection, name)
        case _               => connection.send(ErrorMessage(JoinFirst))
      case Some(seat) => message match
        case JoinTable(_)     => seat.send(ErrorMessage(AlreadySeated))
        case LeaveTable       => leave(seat)
        case PlaceBet(amount) => placeBet(seat, amount)
        case action           => play(seat, action)

  /** For now a disconnection frees the seat like a voluntary leave (mid-hand handling comes with C5). */
  def onDisconnected(connection: Connection): Unit =
    seats.get(connection.id).foreach(leave)

  private def join(connection: Connection, name: String): Unit =
    rejectionReason(name) match
      case Some(reason) => connection.send(JoinRejected(reason))
      case None =>
        val seat = Seat(connection, NormalPlayer(name, initialBalance))
        game.addPlayer(seat.player)
        seats = seats + (connection.id -> seat)
        seat.send(JoinAccepted(name, initialBalance))
        broadcastTable()
        phase match
          case Phase.Idle    => startHand()
          case Phase.Betting => requestBet(seat)
          case Phase.Turn(_) | Phase.DealerTurn => () // a hand is being played: the new player waits for the next one

  /** Why a player with this name cannot sit down, if there is a reason. */
  private def rejectionReason(name: String): Option[String] =
    if seats.size >= Game.MaxPlayersNum then Some(TableFull)
    else if !Game.arePlayersNamesValid(1)(List(name)) then Some(InvalidName) // same rules as the local game
    else if game.isNameValid(name) then Some(NameTaken) // isNameValid = "a player with this name exists"
    else None

  private def leave(seat: Seat): Unit =
    val next = nextToPlay(after = seat.player) // computed while the player is still in the turn order
    game.removeLeavingPlayers(List(seat.player))
    game.currentBets = game.currentBets.filterNot(_.player == seat.player) // a bet left on the table is lost
    seats = seats - seat.connection.id
    if seats.isEmpty then abandonHand()
    else phase match
      case Phase.Betting =>
        broadcastTable()
        dealIfEveryoneHasBet() // the one who left may be the last we waited for
      case Phase.Turn(player) if player == seat.player =>
        nextTurn(next) // it was their turn: it passes on, the table never waits for who left
      case _ =>
        broadcastTable()

  /** A new hand starts: every seated player is asked for a bet. */
  private def startHand(): Unit =
    phase = Phase.Betting
    seats.values.foreach(requestBet)

  private def requestBet(seat: Seat): Unit =
    seat.send(BetRequest(seat.player.balance.totalValue, Game.MinGameBet))

  private def placeBet(seat: Seat, amount: Int): Unit =
    if phase != Phase.Betting then seat.send(ErrorMessage(NotBettingTime))
    else if hasBet(seat.player) then seat.send(ErrorMessage(AlreadyBet))
    else if !game.isBetValid(seat.player)(amount) then seat.send(ErrorMessage(InvalidBet))
    else
      seat.player.withdraw(amount) // same as the local game: the bet leaves the balance when it is placed
      game.currentBets = game.currentBets :+ Bet(seat.player, amount)
      broadcastTable()
      dealIfEveryoneHasBet()

  private def hasBet(player: Player): Boolean = game.currentBets.exists(_.player == player)

  private def dealIfEveryoneHasBet(): Unit =
    if seats.values.forall(seat => hasBet(seat.player)) then deal()

  /** Two cards to every player and to the dealer (the second one of the dealer face down),
   * then the players' turns begin. TableView masks the dealer's face-down card: it never leaves the node. */
  private def deal(): Unit =
    game.distributeCards()
    game.handleBlackjacks(game.initialBlackjackPlayers()) // same as the local game: a blackjack does not play
    nextTurn(game.players.find(needsToPlay))

  /** Whether the player still has to play in this hand: has bet, and is not busted, standing or with a blackjack. */
  private def needsToPlay(player: Player): Boolean =
    hasBet(player) && player.state == PlayerState.Active

  /** The next player in turn order, after the given one, who still has to play. */
  private def nextToPlay(after: Player): Option[Player] =
    game.players.dropWhile(_ != after).drop(1).find(needsToPlay)

  private def nextTurn(next: Option[Player]): Unit = next match
    case Some(player) => startTurn(player)
    case None         => playersDone()

  /** Tells everyone whose turn it is, and asks that player for an action. */
  private def startTurn(player: Player): Unit =
    phase = Phase.Turn(player)
    broadcastTable()
    // double down and split are offered from C4 on
    seatOf(player).foreach(_.send(ActionRequest(player.name, canDoubleDown = false, canSplit = false)))

  /** Only the player whose turn it is may act; every other action is refused. */
  private def play(seat: Seat, action: ClientMessage): Unit = phase match
    case Phase.Turn(player) if player == seat.player => action match
      case Hit   => hit(player)
      case Stand => stand(player)
      case _     => seat.send(ErrorMessage(ActionNotAllowed)) // double down, split, insurance: C4
    case _ => seat.send(ErrorMessage(NotYourTurn))

  /** One more card. Same rules as the local game: bust ends the turn, 21 stands automatically. */
  private def hit(player: Player): Unit =
    game.drawCard(player)
    if game.evaluatePlayerBust(player) then endTurn(player)
    else if player.score.playableValue == WinningScore then stand(player)
    else startTurn(player) // still their turn: they are asked again

  private def stand(player: Player): Unit =
    player.stand()
    endTurn(player)

  private def endTurn(player: Player): Unit = nextTurn(nextToPlay(after = player))

  /** Every player has played: the dealer plays (no choices, it follows the rules), the bets are settled
   * and the hand ends. The whole sequence runs in one go inside the single writer, so no message of a
   * client can slip in between the reveal of the dealer's card and the payout. */
  private def playersDone(): Unit =
    phase = Phase.DealerTurn
    game.resolveInsurances() // same order as the local game; nobody has insurance until C4
    game.computeDealerTurn() // reveals the face-down card, then draws up to 17
    game.handlePayout()
    broadcastTable()         // the dealer's hand is now fully visible, and so are the new balances
    val results = HandResults(game.balances(game.players))
    seats.values.foreach(_.send(results))
    endHand()

  /** Cleans the table as the local game does (split hands, players without money), then starts the next hand. */
  private def endHand(): Unit =
    game.removeSplitPlayers()
    sendGameOverToBrokePlayers()
    game.currentBets = List.empty
    game.handleHandEnd() // every player Active again with an empty hand, empty dealer's hand
    if seats.isEmpty then resetTable()
    else if game.isOver then closeTable() // the cut card has come out: this deck is finished
    else
      broadcastTable()
      startHand()

  /** Players who cannot afford the minimum bet are told the game is over for them and leave the table. */
  private def sendGameOverToBrokePlayers(): Unit =
    val broke = game.brokePlayers()
    broke.flatMap(seatOf).foreach: seat =>
      seat.send(GameOver(game.balances(List(seat.player))))
      seats = seats - seat.connection.id
    game.removeBrokePlayers()

  /** The deck is over: everybody gets the final balances and the table restarts with a fresh game. */
  private def closeTable(): Unit =
    seats.values.foreach(seat => seat.send(GameOver(game.balances(List(seat.player)))))
    resetTable()

  /** Nobody is seated: a new game (with a new deck) is created for the next players. */
  private def resetTable(): Unit =
    seats = Map.empty
    game = newGame()
    phase = Phase.Idle

  /** Nobody is left at the table: the hand is dropped and the table waits for new players. */
  private def abandonHand(): Unit =
    game.currentBets = List.empty
    game.handleHandEnd()
    phase = Phase.Idle

  private def seatOf(player: Player): Option[Seat] = seats.values.find(_.player == player)

  /** Sends the new state of the table, including whose turn it is, to every seated client. */
  private def broadcastTable(): Unit =
    val currentTurn = phase match
      case Phase.Turn(player) => Some(player.name)
      case _                  => None
    val update = TableUpdate(TableView.of(game, currentTurn))
    seats.values.foreach(_.send(update))

object TableHandler:
  /** Balance given to every new player, until balances are persisted per user (D4). */
  val DefaultBalance = 100.0

  val TableFull      = "The table is full"
  val InvalidName    = "The name must not be empty nor contain '_'"
  val NameTaken      = "This name is already used at the table"
  val AlreadySeated  = "You are already seated at this table"
  val JoinFirst      = "Join the table first"
  val NotYourTurn    = "It is not your turn"
  val NotBettingTime = "Bets are not accepted now"
  val AlreadyBet     = "You have already placed your bet for this hand"
  val InvalidBet     = s"The bet must be a positive multiple of ${Game.MinGameBet}, not greater than your balance"
  val ActionNotAllowed = "This action is not allowed now"

  /** The phases of a hand: the table moves from one to the next when the expected messages arrive. */
  private enum Phase:
    case Idle                 // no hand: the next player who sits down starts one
    case Betting              // waiting for the bet of every seated player
    case Turn(player: Player) // cards dealt: waiting for the action of this player
    case DealerTurn           // every player has played: the dealer plays and the bets are settled

  /** A seated player and the connection of its client. */
  private final case class Seat(connection: Connection, player: Player):
    def send(message: ServerMessage): Unit = connection.send(message)
