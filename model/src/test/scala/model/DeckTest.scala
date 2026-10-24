package model

import utils.ModelExports.*
import utils.TestExports.*

class DeckTest extends AnyFunSuite:

  val card: StandardCard = StandardCard(Suit.Hearts, Value.Queen)
  val deck = Deck.standard(2)
  val numSuits = Suit.values.length
  val numValues = Value.values.length
  val shuffledDeck = deck.shuffle(2)

  test("A card can be flipped when requested to be hidden"):
    card.isFaceUp shouldBe true
    card.toString shouldBe
      """┌─────┐
        |│Q  ♥ │
        |└─────┘""".stripMargin


    val flippedCard = card.flip()
    flippedCard.isFaceUp shouldBe false
    flippedCard.toString shouldBe
    """┌─────┐
      |│ ??? │
      |└─────┘""".stripMargin

  test("The standard deck should have the correct number of cards (53)"):
    deck.isEmpty shouldBe false
    deck.size() shouldBe numSuits * numValues + 1

  test("The standard deck should contain the correct number of cards for each suit"):
    for cardType <- List(Suit.Hearts, Suit.Clubs, Suit.Spades, Suit.Diamonds) do
      val count = deck.toList.count:
        case Card.StandardCard(suit, _, _) => suit == cardType
        case _                             => false
      count shouldBe numValues

  test("All cards inside the deck should be face up"):
    val faceDownCards = deck.toList.filter:
      case Card.StandardCard(_, _, isFaceUp) => !isFaceUp
      case _                                 => false
    faceDownCards shouldBe empty

  test("shuffleDeck method should only change order of cards without other side-effects (duplicate or remove cards)"):
    shuffledDeck.size() shouldBe deck.size()
    shuffledDeck.toList.toSet shouldBe deck.toList.toSet

  test("When the deck is shuffled, addCutCardToDeck method should insert the cut card in the same position as before"):
    val numParticipants = 2
    val cutCardDist = numParticipants * 5
    val deckList = shuffledDeck.toList
    val cutCardIndexFromEnd = deckList.length - 1 - deckList.indexOf(Card.CutCard)
    cutCardIndexFromEnd shouldBe cutCardDist

  test("Drawing from a non-empty deck should return the first card and the remaining deck"):
    val expectedCard = shuffledDeck.toList.head
    val (drawnCard, remainingDeck) = shuffledDeck.draw()
    val deckInitialSize = shuffledDeck.size()

    drawnCard.isDefined shouldBe true
    drawnCard.get shouldBe expectedCard
    remainingDeck.size() shouldBe deckInitialSize - 1
    remainingDeck.toList shouldBe shuffledDeck.toList.tail
    remainingDeck.toList.head should not be drawnCard.get
    remainingDeck.toList.filter(c => c == drawnCard.get) shouldBe empty

  test("Drawing from an empty deck should return None and an empty deck"):
    val emptiedDeck = emptyDeck(deck)
    val (drawnCard, remainingDeck) = emptiedDeck.draw()

    drawnCard shouldBe None
    remainingDeck.isEmpty shouldBe true
    remainingDeck.size() shouldBe 0

  test("generateDeck should create a deck composed of multiple standard decks"):
    val numDecks = 3
    val multipleDeck = Deck.generateDeck(numDecks, 2)
    val expectedSize = numSuits * numValues * 3 + 1

    multipleDeck.toList.size shouldBe expectedSize
    val standardCards: List[StandardCard] = for
        suit <- Suit.values.toList
        value <- Value.values.toList
    yield StandardCard(suit, value)

    for expectedCard <- standardCards do
      val cardCount = multipleDeck.toList.count:
        case StandardCard(suit, value, _) => suit == expectedCard.suit && value == expectedCard.value
        case _                            => false
      cardCount shouldBe numDecks

  test("generateDeck should place the CutCard at the exact requested position from the end"):
    val numParticipants = 2
    val cutCardDist = numParticipants * 5
    val multipleDeck = Deck.generateDeck(2, numParticipants)
    val deckList = multipleDeck.toList
    val cutCardIndexFromEnd = deckList.length - 1 - deckList.indexOf(Card.CutCard)
    cutCardIndexFromEnd shouldBe cutCardDist

  test("The standard deck must contain exactly one CutCard"):
    val singleDeck = Deck.standard(3)
    singleDeck.toList.count(_ == Card.CutCard) shouldBe 1

  test("The test deck must be composed only by the sequence of Cards given"):
    val deck = Deck.testDeck(StandardCard(Suit.Spades, Value.Ace), StandardCard(Suit.Spades, Value.Two))
    deck.size() shouldBe 2

  @annotation.tailrec
  final def emptyDeck(deck: Deck): Deck =
    deck.draw() match
      case (None , remainingDeck) =>
        remainingDeck
      case (_, remainingDeck) =>
        emptyDeck(remainingDeck)