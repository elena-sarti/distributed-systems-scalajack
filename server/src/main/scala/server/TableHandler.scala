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

  private val game = newGame()

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
        case _                => seat.send(ErrorMessage(NotYourTurn)) // the players' turns come next

  /** For now a disconnection frees the seat like a voluntary leave (mid-hand handling comes with C5). */
  def onDisconnected(connection: Connection): Unit =
    seats.get(connection.id).foreach(leave)

  // ---- Seats ------------------------------------------------------------------------------------

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
          case Phase.Playing => () // a hand is being played: the new player waits for the next one

  /** Why a player with this name cannot sit down, if there is a reason. */
  private def rejectionReason(name: String): Option[String] =
    if seats.size >= Game.MaxPlayersNum then Some(TableFull)
    else if !Game.arePlayersNamesValid(1)(List(name)) then Some(InvalidName) // same rules as the local game
    else if game.isNameValid(name) then Some(NameTaken) // isNameValid = "a player with this name exists"
    else None

  private def leave(seat: Seat): Unit =
    game.removeLeavingPlayers(List(seat.player))
    game.currentBets = game.currentBets.filterNot(_.player == seat.player) // a bet left on the table is lost
    seats = seats - seat.connection.id
    broadcastTable()
    if seats.isEmpty then abandonHand()
    else if phase == Phase.Betting then dealIfEveryoneHasBet() // the one who left may be the last we waited for

  // ---- Hand: betting and dealing ----------------------------------------------------------------

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

  /** Two cards to every player and to the dealer (the second one of the dealer face down). */
  private def deal(): Unit =
    game.distributeCards()
    phase = Phase.Playing
    broadcastTable() // TableView masks the dealer's face-down card: it never leaves the node

  /** Nobody is left at the table: the hand is dropped and the table waits for new players. */
  private def abandonHand(): Unit =
    game.currentBets = List.empty
    game.handleHandEnd()
    phase = Phase.Idle

  /** Sends the new state of the table to every seated client. */
  private def broadcastTable(): Unit =
    val update = TableUpdate(TableView.of(game))
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

  /** The phases of a hand: the table moves from one to the next when the expected messages arrive. */
  private enum Phase:
    case Idle    // no hand: the next player who sits down starts one
    case Betting // waiting for the bet of every seated player
    case Playing // cards dealt, the hand is being played

  /** A seated player and the connection of its client. */
  private final case class Seat(connection: Connection, player: Player):
    def send(message: ServerMessage): Unit = connection.send(message)
