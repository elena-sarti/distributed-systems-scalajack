package server

import protocol.{ClientMessage, TableView}
import protocol.ClientMessage.*
import protocol.ServerMessage.*
import utils.ModelExports.*

/** The logic of one table: who is seated and how the table reacts to the clients' messages.
 *
 * It is the only authority on the game state: clients send requests, the table validates them,
 * updates the [[Game]] and tells every seated client the new state.
 * NOT thread-safe on purpose: it must be driven by a [[SingleWriterHandler]], so that it is only
 * ever called by one thread and can use plain mutable state.
 *
 * @param initialBalance the balance given to a player when sitting down.
 */
final class TableHandler(initialBalance: Double = TableHandler.DefaultBalance) extends SessionHandler:
  import TableHandler.*

  private val game = Game(List.empty)

  /** The seated players, by connection id. */
  private var seats = Map.empty[String, Seat]

  def onConnected(connection: Connection): Unit = () // nothing to do until the client asks to join

  def onMessage(connection: Connection, message: ClientMessage): Unit =
    (message, seats.get(connection.id)) match
      case (JoinTable(name), None)  => join(connection, name)
      case (JoinTable(_), Some(_))  => connection.send(ErrorMessage(AlreadySeated))
      case (_, None)                => connection.send(ErrorMessage(JoinFirst))
      case (LeaveTable, Some(seat)) => leave(seat)
      case (_, Some(_))             => connection.send(ErrorMessage(NoHandInProgress)) // game actions: C2

  /** For now a disconnection frees the seat like a voluntary leave (mid-hand handling comes with C5). */
  def onDisconnected(connection: Connection): Unit =
    seats.get(connection.id).foreach(leave)

  private def join(connection: Connection, name: String): Unit =
    rejectionReason(name) match
      case Some(reason) => connection.send(JoinRejected(reason))
      case None =>
        val player = NormalPlayer(name, initialBalance)
        game.addPlayer(player)
        seats = seats + (connection.id -> Seat(connection, player))
        connection.send(JoinAccepted(name, initialBalance))
        broadcastTable()

  /** Why a player with this name cannot sit down, if there is a reason. */
  private def rejectionReason(name: String): Option[String] =
    if seats.size >= Game.MaxPlayersNum then Some(TableFull)
    else if !Game.arePlayersNamesValid(1)(List(name)) then Some(InvalidName) // same rules as the local game
    else if game.isNameValid(name) then Some(NameTaken) // isNameValid = "a player with this name exists"
    else None

  private def leave(seat: Seat): Unit =
    game.removeLeavingPlayers(List(seat.player))
    seats = seats - seat.connection.id
    broadcastTable()

  /** Sends the new state of the table to every seated client. */
  private def broadcastTable(): Unit =
    val update = TableUpdate(TableView.of(game))
    seats.values.foreach(_.connection.send(update))

object TableHandler:
  /** Balance given to every new player, until balances are persisted per user (D4). */
  val DefaultBalance = 100.0

  val TableFull        = "The table is full"
  val InvalidName      = "The name must not be empty nor contain '_'"
  val NameTaken        = "This name is already used at the table"
  val AlreadySeated    = "You are already seated at this table"
  val JoinFirst        = "Join the table first"
  val NoHandInProgress = "There is no hand in progress"

  /** A seated player and the connection of its client. */
  private final case class Seat(connection: Connection, player: Player)
