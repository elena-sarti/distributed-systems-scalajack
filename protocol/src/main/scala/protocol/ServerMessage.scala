package protocol

import upickle.default.ReadWriter

/** The decisions of a player that the table waits for and that it takes by itself when the time is up. */
enum Decision derives ReadWriter:
 /** The bet for the next hand. */
  case Bet
   /** The answer to the insurance offer. */
  case Insurance
  /** The action of a hand. */
  case Action

/** Messages that a table node sends to its clients.
 *
 * They are of two kinds:
 *  - requests addressed to a single client, asking for a decision (bet, insurance, action);
 *  - notifications describing what happened at the table (state updates, results, errors).
 */
enum ServerMessage derives ReadWriter:

  /** The player has been seated at the table.
   *
   * @param playerName the name the player is known by at the table.
   * @param balance    the player's balance when sitting down.
   */
  case JoinAccepted(playerName: String, balance: Double)

  /** The player could not be seated.
   *
   * @param reason a human-readable explanation.
   */
  case JoinRejected(reason: String)

  /** How long the player has to answer the request that follows: when the time is
   * over the table decides for them. A number of seconds from now so that the client can count
   * down with its own clock: the clocks of the client and of the table node never have to agree.
   *
   * @param seconds the seconds left, rounded up.
   */
  case TimeToDecide(seconds: Int)

  /** Asks the player to place a bet for the next hand; the client answers with a `PlaceBet`.
   *
   * @param balance the player's current balance, so the client can validate the input.
   * @param minBet  the minimum allowed bet.
   */
  case BetRequest(balance: Double, minBet: Int)

  /** The dealer shows an ace, so it asks the player whether to buy the insurance. */
  case InsuranceOffer

  /** It is the turn of one of the player's hands: asks which action to take.
   * Hit and stand are always allowed; the other actions only when the model allows them.
   *
   * @param handName      the name of the hand that must play (it differs from the player's name after a split).
   * @param canDoubleDown whether a `DoubleDown` would be accepted.
   * @param canSplit      whether a `Split` would be accepted.
   */
  case ActionRequest(handName: String, canDoubleDown: Boolean, canSplit: Boolean)

  /** Broadcast to every client at the table after each state change.
   *
   * @param table a snapshot of the table, safe to be shown to every player.
   */
  case TableUpdate(table: TableView)

  /** The hand is over and the payouts have been applied.
   *
   * @param balances the name and the new balance of every player still at the table
   *                 (same shape returned by the model's `Game.balances`).
   */
  case HandResults(balances: List[(String, Double)])

  /** The game is over (the cut card came out or no human player is left).
   *
   * @param finalBalances the name and the final balance of every player still at the table.
   */
  case GameOver(finalBalances: List[(String, Double)])

  /** The last message received from this client was rejected.
   *
   * @param reason a human-readable explanation (e.g. "it is not your turn").
   */
  case ErrorMessage(reason: String)

  /** The time to decide is over and the table has decided for the player (see [[Decision]] for what it chose).
   * Sent only to the late player, so a client can explain why the game went on without them.
   * 
   * @param decision which decision the player did not take in time.
   */
  case DecisionTimedOut(decision: Decision)