package server.replication

import protocol.ModelCodecs.given
import upickle.default.ReadWriter
import utils.ModelExports.{PlayerState, StandardCard}

/** One hand at the table: a player's own, or one split from it (`split`), with the money it holds. */
final case class HandSnapshot(name: String, split: Boolean, cards: List[StandardCard], balance: Double,
                              state: PlayerState, insured: Boolean) derives ReadWriter

/** A seated player. `connected` is false for a client that went away during the hand. */
final case class SeatSnapshot(player: String, connected: Boolean) derives ReadWriter

/** Where the hand is, with the players referred to by name. */
enum PhaseSnapshot derives ReadWriter:
  case Idle
  case Betting
  case Insurance(awaiting: List[String], accepted: List[String])
  case Turn(hand: String)
  case DealerTurn

/** Everything a table needs to go on from where another copy of it was: the game (hands, bets, dealer's cards,
 * the deck still to deal), who is sitting, where the hand is and when the current wait runs out.
 * The connections are not part of it: they belong to the node.
 *
 * Unlike the snapshots sent to the clients it holds the dealer's face-down card and the whole deck: it only
 * travels between the replicas of a table.
 *
 * @param deck the cards still to deal, in order; `None` is the cut card.
 * @param deadline when the wait for the current decision runs out, in epoch milliseconds.
 */
final case class TableSnapshot(version: Long,
                               hands: List[HandSnapshot],
                               bets: List[(String, Int)],
                               dealerCards: List[StandardCard],
                               deck: List[Option[StandardCard]],
                               cutCardInDeck: Boolean,
                               seats: List[SeatSnapshot],
                               phase: PhaseSnapshot,
                               deadline: Option[Long]) derives ReadWriter
