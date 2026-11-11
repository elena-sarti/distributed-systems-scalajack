package server.replication

import model.DeckModule.toList
import utils.ModelExports.*

/** Turns a game into the hands, bets, dealer's cards and deck of a snapshot, and back. */
object GameSnapshots:

  def hands(game: Game): List[HandSnapshot] = game.players.map: player =>
    val insured = player match
      case normal: NormalPlayer => normal.hasInsurance
      case _                    => false
    HandSnapshot(player.name, player.isInstanceOf[SplitPlayer], player.cards, player.balance.totalValue,
      player.state, insured)

  def bets(game: Game): List[(String, Int)] = game.currentBets.map(bet => bet.player.name -> bet.amount)

  def deck(game: Game): List[Option[StandardCard]] = game.deck.toList.map:
    case card: StandardCard => Some(card)
    case _                  => None // the cut card

  /** The game described by the snapshot. Players and bets refer to the same objects, as in a game played from the start. */
  def rebuild(snapshot: TableSnapshot): Game =
    val players = snapshot.hands.map(player)
    val byName  = players.map(p => p.name -> p).toMap
    val deck    = Deck.fromCards(snapshot.deck.map(_.getOrElse(CutCard)))
    Game.restore(players, deck, snapshot.bets.map((name, amount) => Bet(byName(name), amount)),
      snapshot.dealerCards, snapshot.cutCardInDeck)

  private def player(hand: HandSnapshot): Player =
    val player = hand.cards match
      // a split hand is created with the card it was split with, and that card is always its first one
      case first :: others if hand.split =>
        val split = SplitPlayer(hand.name, first, hand.balance)
        others.foreach(split.addCard)
        split
      case cards =>
        val normal = NormalPlayer(hand.name, hand.balance)
        cards.foreach(normal.addCard)
        normal.hasInsurance = hand.insured
        normal
    hand.state match
      case PlayerState.Standing  => player.stand()
      case PlayerState.Blackjack => player.winBlackjack()
      case PlayerState.Busted    => player.bust()
      case PlayerState.Active    => ()
    player
