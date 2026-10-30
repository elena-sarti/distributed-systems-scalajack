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

  /** Version of the last snapshot sent: it grows by one at every broadcast and is never reset,
   * not even when the table restarts with a new game, so it orders all the states of this table. */
  private var version = 0L

  def onConnected(connection: Connection): Unit = () // nothing to do until the client asks to join

  def onMessage(connection: Connection, message: ClientMessage): Unit =
    seats.get(connection.id) match
      case None => message match
        case JoinTable(name) => join(connection, name)
        case _               => connection.send(ErrorMessage(JoinFirst))
      case Some(seat) => message match
        case JoinTable(_)            => seat.send(ErrorMessage(AlreadySeated))
        case LeaveTable              => leave(seat)
        case PlaceBet(amount)        => placeBet(seat, amount)
        case AnswerInsurance(accept) => answerInsurance(seat, accept)
        case action                  => play(seat, action)

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
          case Phase.Insurance(_, _) | Phase.Turn(_) | Phase.DealerTurn => () // a hand is being played: the new player waits for the next one

  /** Why a player with this name cannot sit down, if there is a reason. */
  private def rejectionReason(name: String): Option[String] =
    if seats.size >= Game.MaxPlayersNum then Some(TableFull)
    else if !Game.arePlayersNamesValid(1)(List(name)) then Some(InvalidName) // same rules as the local game
    else if game.isNameValid(name) then Some(NameTaken) // isNameValid = "a player with this name exists"
    else None

  private def leave(seat: Seat): Unit =
    val hands = handsOf(seat) // the player's own hand and, after a split, the hands split from it
    val next = phase match // computed while the hands are still in the turn order
      case Phase.Turn(current) if hands.contains(current) => nextToPlay(after = current, excluding = hands)
      case _                                              => None
    game.removeLeavingPlayers(hands)
    game.currentBets = game.currentBets.filterNot(bet => hands.contains(bet.player)) // a bet left on the table is lost
    seats = seats - seat.connection.id
    if seats.isEmpty then abandonHand()
    else phase match
      case Phase.Betting =>
        broadcastTable()
        dealIfEveryoneHasBet() // the one who left may be the last we waited for
      case Phase.Insurance(awaiting, accepted) =>
        broadcastTable()
        val name = seat.player.name // the table never waits for who left, and their answer no longer counts
        recordInsuranceAnswers(awaiting - name, accepted.filterNot(_ == name))
      case Phase.Turn(current) if hands.contains(current) =>
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

  /** Two cards to every player and to the dealer (the second one of the dealer face down).
   * If the dealer shows an ace the insurance is offered, otherwise the players' turns begin at once.
   * TableView masks the dealer's face-down card: it never leaves the node. */
  private def deal(): Unit =
    game.distributeCards()
    game.handleBlackjacks(game.initialBlackjackPlayers()) // same as the local game: a blackjack does not play
    if game.dealerHasAce then offerInsurance() else startPlayersTurns()

  /** Every player, in turn order, who still has to play begins: the first one is asked for an action. */
  private def startPlayersTurns(): Unit = nextTurn(game.players.find(needsToPlay))

  /** The dealer shows an ace: every seated player is offered the insurance and the table waits for all the answers. */
  private def offerInsurance(): Unit =
    phase = Phase.Insurance(awaiting = seats.values.map(_.player.name).toSet, accepted = List.empty)
    broadcastTable() // the clients must see their cards and the dealer's ace before they are asked
    seats.values.foreach(_.send(InsuranceOffer))

  private def answerInsurance(seat: Seat, accept: Boolean): Unit = phase match
    case Phase.Insurance(awaiting, accepted) if awaiting.contains(seat.player.name) =>
      val name = seat.player.name
      recordInsuranceAnswers(awaiting - name, if accept then name :: accepted else accepted)
    case _ => seat.send(ErrorMessage(NoInsuranceOffered)) // not offered, or already answered

  /** Keeps the answers received so far. When nobody is left to answer the model applies the insurances
   * (it decides who can afford it, as in the local game) and the players' turns begin. */
  private def recordInsuranceAnswers(awaiting: Set[String], accepted: List[String]): Unit =
    if awaiting.isEmpty then
      game.handleInsurances(accepted)
      startPlayersTurns()
    else phase = Phase.Insurance(awaiting, accepted)

  /** Whether the player still has to play in this hand: has bet, and is not busted, standing or with a blackjack. */
  private def needsToPlay(player: Player): Boolean =
    hasBet(player) && player.state == PlayerState.Active

  /** The next hand in turn order, after the given one, that still has to play (leaving out the excluded hands). */
  private def nextToPlay(after: Player, excluding: List[Player] = List.empty): Option[Player] =
    game.players.dropWhile(_ != after).drop(1).find(hand => needsToPlay(hand) && !excluding.contains(hand))

  private def nextTurn(next: Option[Player]): Unit = next match
    case Some(player) => startTurn(player)
    case None         => playersDone()

  /** Tells everyone whose turn it is, and asks that player for an action. */
  private def startTurn(player: Player): Unit =
    phase = Phase.Turn(player)
    broadcastTable()
    // the rules of the local game decide which actions are offered to the hand
    seatOf(player).foreach(_.send(ActionRequest(player.name, game.canDoubleDown(player), game.canSplit(player))))

  /** Only the player whose turn it is may act, on the hand being played; every other action is refused. */
  private def play(seat: Seat, action: ClientMessage): Unit = phase match
    case Phase.Turn(hand) if belongsTo(hand, seat) => action match
      case Hit        => hit(hand)
      case Stand      => stand(hand)
      case DoubleDown => doubleDown(hand, seat)
      case Split      => split(hand, seat)
      case _          => seat.send(ErrorMessage(ActionNotAllowed))
    case _ => seat.send(ErrorMessage(NotYourTurn))

  /** One more card. Same rules as the local game: bust ends the turn, 21 stands automatically. */
  private def hit(hand: Player): Unit =
    game.drawCard(hand)
    if game.evaluatePlayerBust(hand) then endTurn(hand)
    else if hand.score.playableValue == WinningScore then stand(hand)
    else startTurn(hand) // still their turn: they are asked again

  /** Doubles the bet and draws exactly one card, then the turn ends (bust or stand), as in the local game.
   * The model's rule is checked here because a client may send the message even if it was not offered. */
  private def doubleDown(hand: Player, seat: Seat): Unit =
    if !game.canDoubleDown(hand) then seat.send(ErrorMessage(ActionNotAllowed))
    else
      game.doubleDown(hand)
      if game.evaluatePlayerBust(hand) then endTurn(hand) else stand(hand)

  /** Splits a pair in two hands, each with its own bet and a new card. The player goes on with the first hand
   * (and then plays the new one). As in the local game, a hand that gets a blackjack is paid and does not play. */
  private def split(hand: Player, seat: Seat): Unit =
    if !game.canSplit(hand) then seat.send(ErrorMessage(ActionNotAllowed))
    else
      game.splitPlayer(hand)
      val bothHands = hand :: game.getNextPlayer(hand).toList // the model puts the new hand right after the original one
      val withBlackjack = game.playersWithBlackjack(bothHands)
      game.handleBlackjacks(withBlackjack)
      if withBlackjack.contains(hand) then nextTurn(nextToPlay(after = hand)) else startTurn(hand)

  private def stand(hand: Player): Unit =
    hand.stand()
    endTurn(hand)

  /** The hand is done: its money goes on to the next hand of the same player, if any (so that every hand can
   * afford to double down), then the turn passes to the next hand that has to play. */
  private def endTurn(hand: Player): Unit =
    game.transferBalance(hand)
    nextTurn(nextToPlay(after = hand))

  /** Every player has played: the dealer plays (no choices, it follows the rules), the bets are settled
   * and the hand ends. The whole sequence runs in one go inside the single writer, so no message of a
   * client can slip in between the reveal of the dealer's card and the payout. */
  private def playersDone(): Unit =
    phase = Phase.DealerTurn
    revealDealerCard()
    game.resolveInsurances() // same order as the local game
    game.computeDealerTurn() // the card is already face up: the dealer draws up to 17
    game.handlePayout()
    broadcastTable()         // the dealer's final hand, and the new balances
    val results = HandResults(game.balances(seatedPlayers))
    seats.values.foreach(_.send(results))
    endHand()

  /** The reveal: the face-down card is turned and, only from this moment on, it is part of the
   * snapshots sent to the clients. Before this point it exists only in the memory of the table node. */
  private def revealDealerCard(): Unit =
    game.dealer.revealCards()
    broadcastTable()

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

  /** The name of the player a hand belongs to: a split hand is named after its player ("Alice_split1" is Alice's). */
  private def ownerName(hand: Player): String = hand.name.split("_").head // same convention as the model

  private def belongsTo(hand: Player, seat: Seat): Boolean = ownerName(hand) == seat.player.name

  /** The seat that plays a hand: the player's own hand, or one split from it. */
  private def seatOf(hand: Player): Option[Seat] = seats.values.find(belongsTo(hand, _))

  /** The player's own hand and the hands split from it, in turn order. */
  private def handsOf(seat: Seat): List[Player] = game.players.filter(belongsTo(_, seat))

  /** The players sitting at the table, without the hands split from them: after the payout the money of a
   * split hand has already gone back to its player, so only the player's balance is meaningful. */
  private def seatedPlayers: List[Player] = game.players.filter(player => seats.values.exists(_.player == player))

  /** Sends the new state of the table, including whose turn it is, to every seated client.
   * Everybody gets the very same snapshot, with a new version: since every change goes through the
   * single writer, all the clients see the same states in the same order. */
  private def broadcastTable(): Unit =
    val currentTurn = phase match
      case Phase.Turn(player) => Some(player.name)
      case _                  => None
    version += 1
    val update = TableUpdate(TableView.of(game, currentTurn, version))
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
  val NoInsuranceOffered = "No insurance is offered to you now"

  /** The phases of a hand: the table moves from one to the next when the expected messages arrive. */
  private enum Phase:
    case Idle                 // no hand: the next player who sits down starts one
    case Betting              // waiting for the bet of every seated player
    case Insurance(awaiting: Set[String], accepted: List[String]) // dealer shows an ace: waiting for these players' answers
    case Turn(player: Player) // cards dealt: waiting for the action of this player
    case DealerTurn           // every player has played: the dealer plays and the bets are settled

  /** A seated player and the connection of its client. */
  private final case class Seat(connection: Connection, player: Player):
    def send(message: ServerMessage): Unit = connection.send(message)