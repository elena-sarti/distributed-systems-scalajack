package model

import scala.collection.BuildFrom.buildFromIterableOps
import scala.util.Random

object DeckModule:
  import utils.ModelExports.{StandardCard, addCutCardToDeck}

  /** The four suits of a standard deck of playing cards. */
  enum Suit:
    case Hearts, Spades, Clubs, Diamonds

  /** The possible card values in a standard deck. */
  enum Value:
    case Ace
    case Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten
    case Jack, Queen, King

  /** Represents a card in the desk. It can either be a normal card or a CutCard,
   * used to mark the desk to know when it is time to play the last hand of a game.
   */
  enum Card:
    case StandardCard(suit: Suit, value: Value, isFaceUp: Boolean = true)
    case CutCard

    /** Returns a string representation of the card.
     *
     * If the card is face up, the representation shows its value and suit.
     * Otherwise, the card is hidden.
     *
     * @return the formatted string representation of the card
     */
    override def toString: String = this match
      case StandardCard(suit, value, isFaceUp) =>
        if !isFaceUp then
          """┌─────┐
            |│ ??? │
            |└─────┘""".stripMargin
        else
          val suitSymbol = suit match
            case Suit.Hearts   => "♥"
            case Suit.Diamonds => "♦"
            case Suit.Clubs    => "♣"
            case Suit.Spades   => "♠"
          val valueSymbol = value match
            case Value.Ace   => "A"
            case Value.Jack  => "J"
            case Value.Queen => "Q"
            case Value.King  => "K"
            case Value.Ten   => "10"
            case other => other.ordinal + 1 match
              case n => n.toString
          s"""┌─────┐
             |│$valueSymbol ${if valueSymbol.length == 1 then " " else ""}$suitSymbol │
             |└─────┘""".stripMargin
      case _                                   => """┌─────┐
                                                    |│ CUT │
                                                    |└─────┘""".stripMargin

  extension (c: StandardCard)

    /** Returns a copy of the card with its visibility flipped.
     *
     * @return a card with the opposite face-up state
     */
    def flip(): StandardCard =
      c.copy(isFaceUp = !c.isFaceUp)

  /** Represents a deck of playing cards. */
  opaque type Deck = List[Card]

  object Deck:

    /** Adds a cut card to a deck inserting it at (numParticipants * k) positions to the end of the deck
     *
     * @param deckWithOnlyStdCards The deck made only of standard cards
     * @param numParticipants The number of participants in the game
     * @param k The parameter of the expression, by default = 5
     * @return The deck containing the cut card
     */
    def addCutCardToDeck(deckWithOnlyStdCards: Deck, numParticipants: Int, k: Int = 5): Deck =
      val cutCardPosition = numParticipants * k //per ora proviamo con k = 5
      val (deckWithCutCard, _) = deckWithOnlyStdCards.foldRight((List.empty[Card], 0)):
        case (currentCard, (accList, indexFromEnd)) =>
          if indexFromEnd == cutCardPosition then
            (currentCard :: Card.CutCard :: accList, indexFromEnd + 1)
          else
            (currentCard :: accList, indexFromEnd + 1)
      deckWithCutCard

    /** Generates one or more standard 52-card decks, with a Cut Card distant
     * from the end of the deck by a number of positions equal to the number
     * of participants multiplied by 5, in order to be sure to have enough remaining cards
     * to finish the last hand.
     *
     * @param numDeck the number of decks to generate
     * @param numParticipants the number of participants
     * @return a deck containing all generated cards
     */
    def generateDeck(numDeck: Int, numParticipants: Int): Deck =
      require(numDeck > 0)
      val singleDeck =
        for
          suit  <- Suit.values.toList
          value <- Value.values.toList
        yield StandardCard(suit, value)
      val deckWithOnlyStdCards = List.fill(numDeck)(singleDeck).flatten
      addCutCardToDeck(deckWithOnlyStdCards, numParticipants)

    /** Creates a single standard 52-card deck.
     *
     * @return a standard deck
     */
    def standard(numParticipants: Int): Deck =
      generateDeck(1, numParticipants)

    /** Builds a deck from an explicit sequence of cards, useful for tests */
    def testDeck(cards: Card*): Deck = cards.toList
    
  extension (d: Deck)

    /** Draws the top card from the deck.
     *
     * @return a tuple containing the drawn card, if any, and the remaining deck
     */
    def draw(): (Option[Card], Deck) = d match
      case h :: t => (Some(h), t)
      case _      => (None, d)

    /** Returns a shuffled copy of the deck.
     *
     * @param numParticipants The number of participants to know where to put the cut card.
     * @return a randomly shuffled deck
     */
    def shuffle(numParticipants: Int): Deck =
      val shuffledDeck: Deck = Random.shuffle(d.filterNot(_ == Card.CutCard))
      addCutCardToDeck(shuffledDeck, numParticipants)

    /** Checks whether the deck is empty.
     *
     * @return `true` if the deck contains no cards, `false` otherwise
     */
    def isEmpty: Boolean =
      d.isEmpty

    /** Returns the number of cards currently in the deck.
     *
     * @return the deck size
     */
    def size(): Int =
      d.size

    /** Returns a list of cards.
     *
     * @return the cards contained in the deck
     */
    def toList: List[Card] = d
