package protocol

import utils.TestExports.*
import utils.ModelExports.*

class TableViewTest extends AnyFunSuite with BeforeAndAfterEach:

  var alice: NormalPlayer = _
  var game: Game = _

  // Deal order: Alice, dealer (face up), Alice, dealer (face down).
  // The dealer's face-down card is the only Queen in the deck, so it is easy to look for it in the JSON.
  override def beforeEach(): Unit =
    alice = NormalPlayer("Alice", 100)
    game = Game(List(alice), Deck.testDeck(
      StandardCard(Suit.Hearts, Value.Two),
      StandardCard(Suit.Clubs, Value.Three),
      StandardCard(Suit.Hearts, Value.Four),
      StandardCard(Suit.Spades, Value.Queen)
    ))
    game.distributeCards()

  test("the dealer's face-down card is counted but not shown"):
    val dealer = TableView.of(game).dealer
    dealer.visibleCards shouldBe List(StandardCard(Suit.Clubs, Value.Three))
    dealer.hiddenCards shouldBe 1

  test("the dealer's face-down card never appears in the serialized message"):
    val json = MessageCodec.encode(ServerMessage.TableUpdate(TableView.of(game)))
    json should not include "Queen"

  test("after the reveal all the dealer's cards are visible"):
    game.dealer.revealCards()
    val dealer = TableView.of(game).dealer
    dealer.visibleCards should have size 2
    dealer.hiddenCards shouldBe 0

  test("each player is shown with cards, state, balance and bet"):
    game.currentBets = List(Bet(alice, 10))
    TableView.of(game).players shouldBe List(PlayerView(
      "Alice",
      List(StandardCard(Suit.Hearts, Value.Two), StandardCard(Suit.Hearts, Value.Four)),
      PlayerState.Active,
      100.0,
      Some(10)
    ))

  test("the current turn is reported as given"):
    TableView.of(game, Some("Alice")).currentTurn shouldBe Some("Alice")

  test("the version is reported as given"):
    TableView.of(game, version = 7).version shouldBe 7
