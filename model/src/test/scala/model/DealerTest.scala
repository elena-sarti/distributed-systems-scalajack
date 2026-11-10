package model

import utils.ModelExports.*
import utils.TestExports.*

class DealerTest extends AnyFunSuite with BeforeAndAfterEach:

  var dealer: Dealer = _

  override def beforeEach(): Unit =
    dealer = Dealer()

  test("The dealer should have zero profit initially"):
    dealer.name shouldBe "Dealer"
    dealer.totalProfit shouldBe 0.0

  test("The dealer should increase its profit when adding money"):
    val amountFirstHand = 50.0
    val amountSecondHand = 25.0
    dealer.addProfit(amountFirstHand)
    dealer.totalProfit shouldBe amountFirstHand
    dealer.addProfit(amountSecondHand)
    dealer.totalProfit shouldBe amountFirstHand + amountSecondHand

  test("The dealer should reveal all hidden cards"):
    val hiddenCard: StandardCard = StandardCard(Suit.Hearts, Value.Ace, isFaceUp = false)
    val visibleCard: StandardCard = StandardCard(Suit.Spades, Value.King)
    dealer.addCard(hiddenCard)
    dealer.addCard(visibleCard)
    dealer.revealCards()
    dealer.cards.count(_.isFaceUp ) shouldBe 2

  test("clearHand removes all the cards that the dealer has"):
    val card1: StandardCard = StandardCard(Suit.Hearts, Value.Ace)
    val card2: StandardCard = StandardCard(Suit.Spades, Value.King)
    dealer.addCard(card1)
    dealer.addCard(card2)
    dealer.clearHand()
    dealer.cards.size shouldBe 0

  test("hasFinishedTurn is false when the playable value is below the standing threshold"):
    dealer.addCard(StandardCard(Suit.Hearts, Value.Six))
    dealer.addCard(StandardCard(Suit.Hearts, Value.Six))
    dealer.hasFinishedTurn shouldBe false

  test("hasFinishedTurn is true when the playable value reaches the standing threshold"):
    dealer.addCard(StandardCard(Suit.Hearts, Value.Ten))
    dealer.addCard(StandardCard(Suit.Hearts, Value.Seven))
    dealer.hasFinishedTurn shouldBe true

  test("hasFinishedTurn is false with an ace when the high value busts but the low still below threshold"):
    dealer.addCard(StandardCard(Suit.Hearts, Value.Six))
    dealer.addCard(StandardCard(Suit.Hearts, Value.Six))
    dealer.addCard(StandardCard(Suit.Hearts, Value.Ace))
    dealer.hasFinishedTurn shouldBe false

  test("hasFinishedTurn is true when the hand is already busted with no ace"):
    dealer.addCard(StandardCard(Suit.Hearts, Value.Ten))
    dealer.addCard(StandardCard(Suit.Hearts, Value.Five))
    dealer.addCard(StandardCard(Suit.Hearts, Value.Ten))
    dealer.hasFinishedTurn shouldBe true