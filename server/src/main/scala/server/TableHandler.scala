package server

import protocol.{ClientMessage, Decision, ServerMessage, TableView}
import protocol.ClientMessage.*
import protocol.ServerMessage.*
import server.replication.{GameSnapshots, PhaseSnapshot, SeatSnapshot, TableSnapshot}
import utils.ModelExports.*
import java.time.Instant
import scala.annotation.tailrec
import scala.concurrent.duration.*

/** The logic of one table: who is seated and how a hand is played through the clients' messages.
 *
 * It is the only authority on the game state: clients send requests, the table validates them,
 * updates the [[Game]] and tells every seated client the new state.
 * The flow of a hand is a state machine (see [[TableHandler.Phase]]) moved forward by messages.
 * NOT thread-safe on purpose: it must be driven by a [[SingleWriterHandler]], so that it is only
 * ever called by one thread and can use plain mutable state.
 *
 * @param cashier         gives the balance of a player who sits down, and takes it back when they leave.
 * @param newGame         creates the game of the table (tests pass a game with a known deck).
 * @param clock           the current time (tests pass a clock that they move by hand).
 * @param decisionTimeout how long the table waits for a decision of the players (a bet, an action, the answer to
 *                        the insurance offer) before deciding for who is late.
 */
final class TableHandler(cashier: Cashier = Cashier.withoutMemory(TableHandler.DefaultBalance),
                         newGame: () => Game = () => Game(List.empty),
                         clock: () => Instant = () => Instant.now(),
                         decisionTimeout: FiniteDuration = TableHandler.DefaultDecisionTimeout) extends SessionHandler, SeatAvailability:
  import TableHandler.*

  private var game = newGame()

  /** The seated players, by connection id. Written only by the single writer thread, always through the
   * `seats` setter below, so that the published number of free seats can never be out of date. */
  private var seatsById = Map.empty[String, Seat]

  /** Free seats, published for the threads that are not the writer (e.g. the dispatcher).
   * Volatile: a seat freed by the writer is visible to the readers immediately. */
  @volatile private var publishedFreeSeats = Game.MaxPlayersNum

  private def seats: Map[String, Seat] = seatsById

  /** Every change of the seats (a player sits down, leaves, is removed, the table restarts) ends up here:
   * a single place where the free seats are updated (DRY), so a released seat is available at once. */
  private def seats_=(next: Map[String, Seat]): Unit =
    seatsById = next
    publishedFreeSeats = Game.MaxPlayersNum - next.size

  /** How many players can still sit down. Safe to call from any thread. */
  def freeSeats: Int = publishedFreeSeats

  /** Where the current hand is. */
  private var phase: Phase = Phase.Idle

  /** Version of the last snapshot sent: it grows by one at every broadcast and is never reset,
   * not even when the table restarts with a new game, so it orders all the states of this table. */
  private var version = 0L

  /** When the wait for the current decision runs out, if the table is waiting for one (see [[enter]]). */
  private var deadline: Option[Instant] = None

  /** After a take over (see [[awaitReturn]]): when the players whose clients have not come back give up their seats. */
  private var returnDeadline: Option[Instant] = None

  /** Everything needed to rebuild this table elsewhere (see [[TableSnapshot]]). */
  def snapshot: TableSnapshot =
    val seated = seats.values.map(seat => seat.player.name -> seat.connected).toMap
    val phaseNow = phase match
      case Phase.Idle                          => PhaseSnapshot.Idle
      case Phase.Betting                       => PhaseSnapshot.Betting
      case Phase.Insurance(awaiting, accepted) => PhaseSnapshot.Insurance(awaiting.toList.sorted, accepted)
      case Phase.Turn(hand)                    => PhaseSnapshot.Turn(hand.name)
      case Phase.DealerTurn                    => PhaseSnapshot.DealerTurn
    TableSnapshot(
      version       = version,
      hands         = GameSnapshots.hands(game),
      bets          = GameSnapshots.bets(game),
      dealerCards   = game.dealer.cards,
      deck          = GameSnapshots.deck(game),
      cutCardInDeck = game.isCutCardInDeck,
      seats         = game.players.flatMap(p => seated.get(p.name).map(SeatSnapshot(p.name, _))), // in turn order
      phase         = phaseNow,
      deadline      = deadline.map(_.toEpochMilli)
    )

  /** The connection of every seated player, by name: with it [[restore]] can put the table back as it is now. */
  def connections: Map[String, Connection] = seats.values.map(seat => seat.player.name -> seat.connection).toMap

  /** Takes the state described by the snapshot, in place of the current one.
   *
   * @param connectionOf the connection of a seated player, if there is one on this node (e.g. when the table goes
   *                     back to an earlier state). A player without one keeps the seat, but nothing reaches them until
   *                     their client comes back.
   */
  def restore(snapshot: TableSnapshot, connectionOf: String => Option[Connection] = _ => None): Unit =
    game = GameSnapshots.rebuild(snapshot)
    val players = game.players.map(p => p.name -> p).toMap
    val restoredSeats = snapshot.seats.map: seat =>
      val connection = connectionOf(seat.player).getOrElse(Absent(seat.player))
      connection.id -> Seat(connection, players(seat.player), seat.connected)
    seats = restoredSeats.toMap
    phase = snapshot.phase match
      case PhaseSnapshot.Idle                          => Phase.Idle
      case PhaseSnapshot.Betting                       => Phase.Betting
      case PhaseSnapshot.Insurance(awaiting, accepted) => Phase.Insurance(awaiting.toSet, accepted)
      case PhaseSnapshot.Turn(hand)                    => Phase.Turn(players(hand))
      case PhaseSnapshot.DealerTurn                    => Phase.DealerTurn
    version = snapshot.version
    deadline = snapshot.deadline.map(Instant.ofEpochMilli)

  def onConnected(connection: Connection): Unit = () // nothing to do until the client asks to join

  def onMessage(connection: Connection, message: ClientMessage): Unit =
    seats.get(connection.id) match
      case None => message match
        // a player who was seated before the take over comes back to their seat; anybody else sits down
        case JoinTable(name) => waitingSeat(name).fold(join(connection, name))(resume(connection, _))
        case _               => connection.send(ErrorMessage(JoinFirst))
      case Some(seat) => message match
        case JoinTable(_)            => seat.send(ErrorMessage(AlreadySeated))
        case LeaveTable              => leave(seat)
        case PlaceBet(amount)        => placeBet(seat, amount)
        case AnswerInsurance(accept) => answerInsurance(seat, accept)
        case action                  => play(seat, action)

  def onDisconnected(connection: Connection): Unit =
    seats.get(connection.id).foreach(disconnect)

  /** The client is gone. If a hand is being played the seat is kept until the hand is over: the player's hands
   * stand by themselves (the bets are settled like the others') and the table never waits for them.
   * Otherwise there is nothing to settle and the seat is freed at once. */
  private def disconnect(seat: Seat): Unit = phase match
    case Phase.Insurance(awaiting, accepted) =>
      keepUntilEndOfHand(seat)
      recordInsuranceAnswers(awaiting - seat.player.name, accepted) // no answer: no insurance (unless already bought)
    case Phase.Turn(current) =>
      keepUntilEndOfHand(seat)
      if belongsTo(current, seat) then playForAbsent(current) // it was their turn: the table finishes the hand now
    // otherwise the table plays their hands when their turn comes (see startTurn)
    case _ => leave(seat) // betting, or no hand at all

  /** The seat stays, but nothing is sent to it any more and its hands are played automatically. */
  private def keepUntilEndOfHand(seat: Seat): Unit =
    seats = seats.updated(seat.connection.id, seat.copy(connected = false))

  /** Time passes. If the table has been waiting too long for a decision it decides for who is late (no bet: the player
   * sits out of the hand; no answer to the insurance: no insurance; no action: the table plays the hand, see
   * [[playForAbsent]]), so a client that stays silent can never block the others. */
  override def onTick(): Unit =
    returnDeadline.filter(limit => !clock().isBefore(limit)).foreach(_ => giveUpWaiting())
    if deadline.exists(limit => !clock().isBefore(limit)) then decisionTimedOut()

  /** The table has been taken over from a failed node: the seated players are still here, but their clients are not.
   * For `window` the table waits for them to come back (see [[resume]]) and does not decide for anybody before that;
   * then the seats of the ones who did not come back are given up like those of a client that drops (C5), and
   * the table never blocks for them.
   *
   * Call it right after [[restore]], before the table is used. */
  def awaitReturn(window: FiniteDuration): Unit =
    val limit = clock().plusMillis(window.toMillis)
    returnDeadline = Some(limit)
    // the old deadline is the failed node's: nobody can be late for a decision before they have had the time to come back
    deadline = deadline.map(current => if current.isBefore(limit) then limit else current)

  /** The seat of a player whose client has not come back yet, if there is one. */
  private def waitingSeat(name: String): Option[Seat] = seats.get(Absent(name).id).filter(_.connected)

  /** A client comes back to the seat it had before the take over: it gets its seat, the table as it is and,
   * if the table is waiting for something from it, the same request it was answering, so the hand goes on from the
   * point where it was. Nothing goes through the cashier: the player is already seated, with their balance. */
  private def resume(connection: Connection, waiting: Seat): Unit =
    val seat = waiting.copy(connection = connection)
    seats = seats - waiting.connection.id + (connection.id -> seat)
    seat.send(JoinAccepted(seat.player.name, seat.player.balance.totalValue))
    broadcastTable() // the returned client sees the table, the others see that it is back
    askAgain(seat)

  /** Repeats to the seat what the table is waiting for from it right now, if anything (the request was sent to the old node). */
  private def askAgain(seat: Seat): Unit = phase match
    case Phase.Betting if !hasBet(seat.player) => requestBet(seat)
    case Phase.Insurance(awaiting, _) if awaiting.contains(seat.player.name) => ask(seat, InsuranceOffer)
    case Phase.Turn(hand) if belongsTo(hand, seat) =>
      ask(seat, ActionRequest(hand.name, game.canDoubleDown(hand), game.canSplit(hand)))
    case _ => ()

  /** The time to come back is over: the players still absent are treated as disconnected. If no cards have been dealt
   * the failure is not their fault, so the bets they placed go back to them; from the deal on, their hands stand. */
  private def giveUpWaiting(): Unit =
    returnDeadline = None
    val absent = seats.values.filter(seat => seat.connected && seat.connection.isInstanceOf[Absent]).toList
    absent.foreach: gone =>
      seats.get(gone.connection.id).foreach: seat => // the seats change while the others leave: look again
        phase match
          case Phase.Idle | Phase.Betting => leave(seat, refund = betsOf(seat))
          case _                          => disconnect(seat)

  /** What the player has bet on the table in this hand. */
  private def betsOf(seat: Seat): Double =
    val hands = handsOf(seat)
    game.currentBets.filter(bet => hands.contains(bet.player)).map(_.amount).sum

  private def decisionTimedOut(): Unit = phase match
    case Phase.Betting => betTimedOut()
    case Phase.Insurance(awaiting, accepted) =>
      notifyTimedOut(seats.values.filter(seat => awaiting.contains(seat.player.name)), Decision.Insurance)
      recordInsuranceAnswers(Set.empty, accepted) // who has not answered declines
    case Phase.Turn(hand) =>
      notifyTimedOut(seatOf(hand).toList, Decision.Action) // first, so the client reads "time is up" before the cards
      playForAbsent(hand)
    case _                            => deadline = None

  /** The time to bet is over: the hand starts with the players who have bet, and who has not sits it out
   * (keeps the seat and the money, and is asked again at the next hand). If nobody has bet there is nothing
   * to deal: a new round of bets begins. */
  private def betTimedOut(): Unit =
    notifyTimedOut(seats.values.filterNot(seat => hasBet(seat.player)), Decision.Bet)
    if game.currentBets.isEmpty then startHand() else deal()

  /** Tells the late players which of their decisions the table has taken for them. */
  private def notifyTimedOut(late: Iterable[Seat], decision: Decision): Unit =
    late.foreach(_.send(DecisionTimedOut(decision)))

  /** Moves the table to a new phase. The phases in which the table waits for a decision of the players get a
   * deadline: when it passes, [[onTick]] decides for who is late. */
  private def enter(next: Phase): Unit =
    phase = next
    deadline = next match
      case Phase.Betting | Phase.Insurance(_, _) | Phase.Turn(_) => Some(clock().plusMillis(decisionTimeout.toMillis))
      case _                                     => None

  private def join(connection: Connection, name: String): Unit =
    rejectionReason(name).toLeft(()).flatMap(_ => cashier.sitDown(name)) match
      case Left(reason) => connection.send(JoinRejected(reason))
      case Right(balance) if balance < Game.MinGameBet =>
        cashier.standUp(name, balance) // the money stays where it was
        connection.send(JoinRejected(NotEnoughMoney))
      case Right(balance) =>
        val seat = Seat(connection, NormalPlayer(name, balance))
        game.addPlayer(seat.player)
        seats = seats + (connection.id -> seat)
        seat.send(JoinAccepted(name, balance))
        broadcastTable()
        phase match
          case Phase.Idle    => startHand()
          case Phase.Betting => restartBetting()
          case Phase.Insurance(_, _) | Phase.Turn(_) | Phase.DealerTurn => () // a hand is being played: the new player waits for the next one

  /** Why a player with this name cannot sit down, if there is a reason. */
  private def rejectionReason(name: String): Option[String] =
    if seats.size >= Game.MaxPlayersNum then Some(TableFull)
    else if !Game.arePlayersNamesValid(1)(List(name)) then Some(InvalidName) // same rules as the local game
    else if game.isNameValid(name) then Some(NameTaken) // isNameValid = "a player with this name exists"
    else None

  /** @param refund money that goes back to the player besides their balance (the bet that was on the table). */
  private def leave(seat: Seat, refund: Double = 0.0): Unit =
    val hands = handsOf(seat) // the player's own hand and, after a split, the hands split from it
    val next = phase match // computed while the hands are still in the turn order
      case Phase.Turn(current) if hands.contains(current) => nextToPlay(after = current, excluding = hands)
      case _                                              => None
    val money = hands.map(_.balance.totalValue).sum // after a split part of it may be on the other hands
    game.removeLeavingPlayers(hands)
    game.currentBets = game.currentBets.filterNot(bet => hands.contains(bet.player)) // a bet left on the table is lost
    seats = seats - seat.connection.id
    standUp(seat, money + refund)
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
    enter(Phase.Betting)
    seats.values.foreach(requestBet)

  /** A new player sits down while the others are betting: the time to bet starts again, so that the newcomer has all
   * of it too, and whoever has not bet yet is asked again with the new time. */
  private def restartBetting(): Unit =
    enter(Phase.Betting)
    seats.values.filterNot(seat => hasBet(seat.player)).foreach(requestBet)

  private def requestBet(seat: Seat): Unit =
    ask(seat, BetRequest(seat.player.balance.totalValue, Game.MinGameBet))

  /** Sends a request together with the time left to answer it: the deadline of the phase the table is in. */
  private def ask(seat: Seat, request: ServerMessage): Unit =
    deadline.foreach: limit =>
      val millisLeft = math.max(0L, limit.toEpochMilli - clock().toEpochMilli)
      seat.send(TimeToDecide(((millisLeft + 999) / 1000).toInt))
    seat.send(request)

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

  /** Two cards to every player who has bet (a seated player without a bet sits out) and to the dealer
   * (the second one of the dealer face down).
   * If the dealer shows an ace the insurance is offered, otherwise the players' turns begin at once.
   * TableView masks the dealer's face-down card: it never leaves the node. */
  private def deal(): Unit =
    game.distributeCards(game.players.filter(hasBet))
    game.handleBlackjacks(game.initialBlackjackPlayers()) // same as the local game: a blackjack does not play
    if game.dealerHasAce then offerInsurance() else startPlayersTurns()

  /** Every player, in turn order, who still has to play begins: the first one is asked for an action. */
  private def startPlayersTurns(): Unit = nextTurn(game.players.find(needsToPlay))

  /** The dealer shows an ace: every player who bet is offered the insurance and the table waits for all the answers. */
  private def offerInsurance(): Unit =
    val offered = seats.values.filter(seat => hasBet(seat.player))
    enter(Phase.Insurance(awaiting = offered.map(_.player.name).toSet, accepted = List.empty))
    broadcastTable() // the clients must see their cards and the dealer's ace before they are asked
    offered.foreach(ask(_, InsuranceOffer))

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
    else phase = Phase.Insurance(awaiting, accepted) // same deadline for everybody: no new [[enter]]

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
    enter(Phase.Turn(player))
    seatOf(player) match
      case Some(seat) if !seat.connected => playForAbsent(player) // the client is gone: nobody to ask
      case seat =>
        broadcastTable()
        // the rules of the local game decide which actions are offered to the hand
        seat.foreach(ask(_, ActionRequest(player.name, game.canDoubleDown(player), game.canSplit(player))))

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

  /** The player does not answer (time is up, or the client is gone) but has a bet on the hand: the table plays it as
   * live casinos do, with the safe rule: one more card while the hand is worth less than 12 (no card can bust it),
   * then it stands. */
  private def playForAbsent(hand: Player): Unit =
    if hand.score.playableValue >= AutoStandFrom then stand(hand)
    else game.drawCard(hand) match
      case None                                                 => stand(hand) // no card left to draw
      case Some(_) if game.evaluatePlayerBust(hand)             => endTurn(hand)
      case Some(_) if hand.score.playableValue == WinningScore => stand(hand)
      case Some(_) =>
        broadcastTable() // everybody sees the card drawn for the absent player
        playForAbsent(hand)

  /** The hand is done: its money goes on to the next hand of the same player, if any (so that every hand can
   * afford to double down), then the turn passes to the next hand that has to play. */
  private def endTurn(hand: Player): Unit =
    game.transferBalance(hand)
    nextTurn(nextToPlay(after = hand))

  /** Every player has played: the dealer plays (no choices, it follows the rules), the bets are settled
   * and the hand ends. The whole sequence runs in one go inside the single writer, so no message of a
   * client can slip in between the reveal of the dealer's card and the payout. */
  private def playersDone(): Unit =
    enter(Phase.DealerTurn)
    revealDealerCard()
    game.resolveInsurances() // same order as the local game
    dealerDraws()
    game.handlePayout()
    broadcastTable()         // the dealer's final hand, and the new balances
    val results = HandResults(game.balances(seatedPlayers))
    seats.values.foreach(_.send(results))
    endHand()

  /** The dealer draws up to 17, one card at a time, and the clients see every card as it comes: the last one arrives
   * with the payout. The same rule as the local game (computeDealerTurn), which draws them all at once. */
  @tailrec
  private def dealerDraws(): Unit =
    if !game.dealer.hasFinishedTurn && game.drawCard(game.dealer).isDefined then
      if !game.dealer.hasFinishedTurn then broadcastTable()
      dealerDraws()

  /** The reveal: the face-down card is turned and, only from this moment on, it is part of the
   * snapshots sent to the clients. Before this point it exists only in the memory of the table node. */
  private def revealDealerCard(): Unit =
    game.dealer.revealCards()
    broadcastTable()

  /** Cleans the table as the local game does (split hands, players without money), then starts the next hand. */
  private def endHand(): Unit =
    game.removeSplitPlayers()
    removeDisconnectedSeats()
    sendGameOverToBrokePlayers()
    game.currentBets = List.empty
    game.handleHandEnd() // every player Active again with an empty hand, empty dealer's hand
    if seats.isEmpty then resetTable()
    else if game.isOver then closeTable() // the cut card has come out: this deck is finished
    else
      broadcastTable()
      startHand()

  /** The bets are settled: the seats of the clients that disconnected during the hand are freed now. */
  private def removeDisconnectedSeats(): Unit =
    val gone = seats.values.filterNot(_.connected).toList
    game.removeLeavingPlayers(gone.map(_.player))
    seats = seats -- gone.map(_.connection.id)
    gone.foreach(seat => standUp(seat, seat.player.balance.totalValue))

  /** Players who cannot afford the minimum bet are told the game is over for them and leave the table. */
  private def sendGameOverToBrokePlayers(): Unit =
    val broke = game.brokePlayers()
    broke.flatMap(seatOf).foreach: seat =>
      seat.send(GameOver(game.balances(List(seat.player))))
      seats = seats - seat.connection.id
      standUp(seat, seat.player.balance.totalValue)
    game.removeBrokePlayers()

  /** The deck is over: everybody gets the final balances and the table restarts with a fresh game. */
  private def closeTable(): Unit =
    seats.values.foreach: seat =>
      seat.send(GameOver(game.balances(List(seat.player))))
      standUp(seat, seat.player.balance.totalValue)
    resetTable()

  /** The node is shutting down: a hand in progress is cancelled, so the bets go back to the players, and
   * everybody leaves with their money. */
  override def onStop(): Unit =
    seats.values.foreach: seat =>
      val hands = handsOf(seat)
      val bets  = game.currentBets.filter(bet => hands.contains(bet.player)).map(_.amount).sum
      standUp(seat, hands.map(_.balance.totalValue).sum + bets)
    resetTable()

  private def standUp(seat: Seat, balance: Double): Unit = cashier.standUp(seat.player.name, balance)

  /** Nobody is seated: a new game (with a new deck) is created for the next players. */
  private def resetTable(): Unit =
    seats = Map.empty
    game = newGame()
    enter(Phase.Idle)

  /** Nobody is left at the table: the hand is dropped and the table waits for new players. */
  private def abandonHand(): Unit =
    game.currentBets = List.empty
    game.handleHandEnd()
    enter(Phase.Idle)

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
  /** Balance of every player at a table that does not keep the balances (see [[Cashier.withoutMemory]]). */
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
  val NotEnoughMoney     = s"You need at least ${Game.MinGameBet} to sit down"

  /** A hand played by the table for a player who does not answer gets cards until it is worth at least this much. */
  val AutoStandFrom = 12

  /** How long the table waits for a decision before deciding for the player. */
  val DefaultDecisionTimeout: FiniteDuration = 30.seconds

  /** The phases of a hand: the table moves from one to the next when the expected messages arrive. */
  private enum Phase:
    case Idle                 // no hand: the next player who sits down starts one
    case Betting              // waiting for the bet of every seated player
    case Insurance(awaiting: Set[String], accepted: List[String]) // dealer shows an ace: waiting for these players' answers
    case Turn(player: Player) // cards dealt: waiting for the action of this player
    case DealerTurn           // every player has played: the dealer plays and the bets are settled

  /** Stands in for the connection of a seated player whose client is not connected to this node: messages are dropped. */
  private final case class Absent(player: String) extends Connection:
    val id: String = s"absent:$player"
    def send(message: ServerMessage): Unit = ()

  /** A seated player and the connection of its client. After a disconnection during a hand the seat is kept
   * until the end of the hand with `connected = false`: nothing is sent to it any more. */
  private final case class Seat(connection: Connection, player: Player, connected: Boolean = true):
    def send(message: ServerMessage): Unit = if connected then connection.send(message)