package protocol

import upickle.default.ReadWriter

/** Messages that a client sends to a table node. */
enum ClientMessage derives ReadWriter: //ReadWriter allows to serialize and deserialize into JSON format.

  /** The request of a player to sit at a table.
   *
   * @param playerName the name shown to the other players at the table.
   */
  case JoinTable(playerName: String)

  /** The amount the player wants to bet for this hand.
   *
   * @param amount the amount of the bet.
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

  /** Answer to a request of insurance deal, when the dealer shows an Ace.
   *
   * @param accept `true` if the player wants to buy the insurance.
   */
  case AnswerInsurance(accept: Boolean)

  /** The player voluntarily leaves the table: the seat is released,
   *  a bet already placed is lost and the table never waits for them again. */
  case LeaveTable
