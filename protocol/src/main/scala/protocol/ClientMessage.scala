package protocol

import upickle.default.ReadWriter

/** Messages that a client (the player's terminal) sends to a table node.
 *
 * The client never changes the game state by itself: every message is a *request*
 * that the table node (the only authority on the game state) validates and applies.
 * `derives ReadWriter` asks upickle to generate the JSON (de)serializer at compile time.
 */
enum ClientMessage derives ReadWriter:

  /** First message of a session: the player asks to sit at the table.
   *
   * @param playerName the name shown to the other players at the table.
   */
  case JoinTable(playerName: String)

  /** Answer to a [[ServerMessage.BetRequest]]: the amount the player wants to bet for this hand.
   *
   * @param amount the amount of the bet (same unit as the model's `Bet.amount`).
   */
  case PlaceBet(amount: Int)

  /** Asks for one more card for the hand whose turn it is. */
  case Hit

  /** Ends the turn of the current hand. */
  case Stand

  /** Doubles the bet of the current hand and draws exactly one more card. */
  case DoubleDown

  /** Splits the current hand into two separate hands. */
  case Split

  /** Answer to a [[ServerMessage.InsuranceOffer]].
   *
   * @param accept `true` if the player wants to buy the insurance.
   */
  case AnswerInsurance(accept: Boolean)

  /** The player voluntarily leaves the table; the seat is released at the end of the hand. */
  case LeaveTable
