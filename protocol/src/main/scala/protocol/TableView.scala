package protocol

import upickle.default.ReadWriter
import protocol.ModelCodecs.given
import utils.ModelExports.*

/** What every client can see about a single player (or split hand) at the table.
 * Cards and state reuse the model types directly, no separate DTOs are introduced.
 *
 * @param name    the name of the player, or of the hand after a split (e.g. "Alice_split1").
 * @param cards   the cards in the hand.
 * @param state   the state of the hand (active, standing, busted, blackjack).
 * @param balance the current balance of the player.
 * @param bet     the bet placed in the current hand, if any.
 */
final case class PlayerView(name: String,
                            cards: List[StandardCard],
                            state: PlayerState,
                            balance: Double,
                            bet: Option[Int]) derives ReadWriter

/** What every client can see about the dealer.
 *
 * Face-down cards are *not* included: only how many there are. This way the hidden card
 * never leaves the table node, so it cannot be read by sniffing the network traffic.
 *
 * @param visibleCards the face-up cards of the dealer.
 * @param hiddenCards  how many face-down cards the dealer holds.
 */
final case class DealerView(visibleCards: List[StandardCard], hiddenCards: Int) derives ReadWriter

/** A snapshot of the whole table, broadcast to every client after each change of state.
 *
 * @param players     the players at the table, in turn order.
 * @param dealer      the dealer, with the face-down cards masked.
 * @param currentTurn the name of the hand whose turn it is, if a turn is in progress.
 */
final case class TableView(players: List[PlayerView],
                           dealer: DealerView,
                           currentTurn: Option[String]) derives ReadWriter

object TableView:

  /** Builds the public snapshot of a game, hiding the dealer's face-down cards.
   *
   * @param game        the game held by the table node.
   * @param currentTurn the name of the hand whose turn it is, if any.
   * @return the snapshot that can be safely sent to every client.
   */
  def of(game: Game, currentTurn: Option[String] = None): TableView =
    val players = game.players.map: player =>
      val bet = game.currentBets.find(_.player == player).map(_.amount)
      PlayerView(player.name, player.cards, player.state, player.balance.totalValue, bet)
    val (faceUp, faceDown) = game.dealer.cards.partition(_.isFaceUp)
    TableView(players, DealerView(faceUp, faceDown.size), currentTurn)
