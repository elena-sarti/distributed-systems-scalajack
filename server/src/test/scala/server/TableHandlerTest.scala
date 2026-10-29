package server

import utils.TestExports.*
import utils.ModelExports.{CutCard, Deck, Game, PlayerState, StandardCard, Suit, Value}
import protocol.{ServerMessage, TableView}
import protocol.ClientMessage.*
import protocol.ServerMessage.*
import server.TableHandler.*
import java.util.UUID
import scala.collection.mutable.ListBuffer

/** The table logic is tested without network nor threads: it is called directly,
 * as the single writer thread would do, with fake connections that record what they receive. */
class TableHandlerTest extends AnyFunSuite with BeforeAndAfterEach:

  private final class FakeConnection(val id: String) extends Connection:
    val inbox = ListBuffer[ServerMessage]()
    def send(message: ServerMessage): Unit = inbox += message

  var table: TableHandler = _

  override def beforeEach(): Unit =
    table = TableHandler()

  /** A table whose deck deals exactly the given cards, in this order. */
  private def tableWithDeck(cards: StandardCard*): TableHandler =
    TableHandler(newGame = () => Game(List.empty, Deck.testDeck(cards*)))

  /** A new client that asks to sit with the given name. */
  private def join(name: String, at: TableHandler = table): FakeConnection =
    val connection = FakeConnection(UUID.randomUUID().toString) // every client is a different connection
    at.onConnected(connection)
    at.onMessage(connection, JoinTable(name))
    connection

  /** The last table update received by the client. */
  private def lastTable(connection: FakeConnection): TableView =
    connection.inbox.collect { case TableUpdate(view) => view }.last

  private def seatedPlayers(connection: FakeConnection): List[String] =
    lastTable(connection).players.map(_.name)

  test("a player who joins is accepted with the initial balance and sees the table"):
    val alice = join("Alice")
    alice.inbox.head shouldBe JoinAccepted("Alice", DefaultBalance)
    seatedPlayers(alice) shouldBe List("Alice")

  test("every seated player is told when someone joins"):
    val alice = join("Alice")
    join("Bob")
    seatedPlayers(alice) shouldBe List("Alice", "Bob")

  test("a name already used at the table is rejected"):
    join("Alice")
    join("Alice").inbox.toList shouldBe List(JoinRejected(NameTaken))

  test("an empty name or a name with '_' is rejected"):
    join("").inbox.toList shouldBe List(JoinRejected(InvalidName))
    join("Al_ice").inbox.toList shouldBe List(JoinRejected(InvalidName))

  test("when the table is full a new player is rejected"):
    (1 to Game.MaxPlayersNum).foreach(i => join(s"Player$i"))
    join("Late").inbox.toList shouldBe List(JoinRejected(TableFull))

  test("a client cannot join twice"):
    val alice = join("Alice")
    table.onMessage(alice, JoinTable("AliceAgain"))
    alice.inbox.last shouldBe ErrorMessage(AlreadySeated)

  test("a client that has not joined cannot play"):
    val stranger = FakeConnection("stranger")
    table.onMessage(stranger, Hit)
    stranger.inbox.toList shouldBe List(ErrorMessage(JoinFirst))

  test("a player who leaves frees the seat and the others are told"):
    val alice = join("Alice")
    val bob   = join("Bob")
    table.onMessage(bob, LeaveTable)
    seatedPlayers(alice) shouldBe List("Alice")
    join("Bob").inbox.head shouldBe JoinAccepted("Bob", DefaultBalance) // the name is free again

  test("a disconnection frees the seat like a voluntary leave"):
    val alice = join("Alice")
    val bob   = join("Bob")
    table.onDisconnected(bob)
    seatedPlayers(alice) shouldBe List("Alice")

  test("the disconnection of a client that never joined changes nothing"):
    val alice = join("Alice")
    val messagesBefore = alice.inbox.size
    table.onDisconnected(FakeConnection("stranger"))
    alice.inbox.size shouldBe messagesBefore

  test("when the first player sits down a hand starts and the bet is requested"):
    val alice = join("Alice")
    alice.inbox.last shouldBe BetRequest(DefaultBalance, Game.MinGameBet)

  test("a player who sits down while the others are betting is asked to bet too"):
    join("Alice")
    join("Bob").inbox.last shouldBe BetRequest(DefaultBalance, Game.MinGameBet)

  test("a valid bet is taken from the balance and shown to the whole table"):
    val alice = join("Alice")
    val bob   = join("Bob")
    table.onMessage(alice, PlaceBet(10))
    val aliceAsSeenByBob = lastTable(bob).players.find(_.name == "Alice").get
    aliceAsSeenByBob.bet shouldBe Some(10)
    aliceAsSeenByBob.balance shouldBe DefaultBalance - 10

  test("an invalid bet is refused and the player can bet again"):
    val alice = join("Alice")
    table.onMessage(alice, PlaceBet(3))    // not a multiple of the minimum bet
    alice.inbox.last shouldBe ErrorMessage(InvalidBet)
    table.onMessage(alice, PlaceBet(1000)) // more than the balance
    alice.inbox.last shouldBe ErrorMessage(InvalidBet)
    table.onMessage(alice, PlaceBet(10))
    lastTable(alice).players.head.bet shouldBe Some(10)

  test("a player cannot bet twice in the same hand"):
    val alice = join("Alice")
    join("Bob") // keeps the table in the betting phase after Alice's bet
    table.onMessage(alice, PlaceBet(10))
    table.onMessage(alice, PlaceBet(10))
    alice.inbox.last shouldBe ErrorMessage(AlreadyBet)

  test("game actions are refused when it is not the player's turn"):
    val alice = join("Alice")
    table.onMessage(alice, Hit)
    alice.inbox.last shouldBe ErrorMessage(NotYourTurn)

  private val two: StandardCard   = StandardCard(Suit.Hearts, Value.Two)
  private val three: StandardCard = StandardCard(Suit.Clubs, Value.Three)
  private val four: StandardCard  = StandardCard(Suit.Hearts, Value.Four)
  private val queen: StandardCard = StandardCard(Suit.Spades, Value.Queen)

  test("when every seated player has bet the cards are dealt, with the dealer's second card hidden"):
    val dealt = tableWithDeck(two, three, four, queen) // Alice, dealer, Alice, dealer (face down)
    val alice = join("Alice", dealt)
    dealt.onMessage(alice, PlaceBet(10))
    val view = lastTable(alice)
    view.players.head.cards shouldBe List(two, four)
    view.dealer.visibleCards shouldBe List(three)
    view.dealer.hiddenCards shouldBe 1

  test("the deal waits for the bet of every seated player"):
    val alice = join("Alice")
    val bob   = join("Bob")
    table.onMessage(alice, PlaceBet(10))
    lastTable(alice).dealer.visibleCards.isEmpty shouldBe true
    table.onMessage(bob, PlaceBet(10))
    lastTable(alice).dealer.visibleCards.size shouldBe 1

  test("a player who leaves during the betting is not waited for"):
    val alice = join("Alice")
    val bob   = join("Bob")
    table.onMessage(alice, PlaceBet(10))
    table.onMessage(bob, LeaveTable)
    lastTable(alice).dealer.visibleCards.size shouldBe 1

  test("bets are refused once the cards are dealt"):
    val alice = join("Alice")
    table.onMessage(alice, PlaceBet(10))
    table.onMessage(alice, PlaceBet(10))
    alice.inbox.last shouldBe ErrorMessage(NotBettingTime)

  test("if everybody leaves, the hand is dropped and the next player starts a new one"):
    val alice = join("Alice")
    table.onMessage(alice, PlaceBet(10))
    table.onMessage(alice, LeaveTable)
    val bob = join("Bob")
    bob.inbox.last shouldBe BetRequest(DefaultBalance, Game.MinGameBet)
    lastTable(bob).dealer.visibleCards.isEmpty shouldBe true

  private def card(value: Value): StandardCard = StandardCard(Suit.Diamonds, value)

  /** A table with Alice already seated, her bet of 10 placed and the cards dealt from `cards`
   * (deal order: Alice, dealer, Alice, dealer face down, then the cards drawn later). */
  private def aliceDealt(cards: Value*): (TableHandler, FakeConnection) =
    val dealt = tableWithDeck(cards.map(card)*)
    val alice = join("Alice", dealt)
    dealt.onMessage(alice, PlaceBet(10))
    (dealt, alice)

  /** The table as it was when the hand ended: the last update before the results of the hand
   * (afterwards the table is cleaned for the next hand). */
  private def tableAtEndOfHand(connection: FakeConnection): TableView =
    val beforeResults = connection.inbox.toList.takeWhile(!_.isInstanceOf[HandResults])
    beforeResults.collect { case TableUpdate(view) => view }.last

  private def handResults(connection: FakeConnection): List[(String, Double)] =
    connection.inbox.collectFirst { case HandResults(balances) => balances }.get

  private def aliceState(alice: FakeConnection): PlayerState = tableAtEndOfHand(alice).players.head.state

  test("after the deal the first player is asked to act and everyone sees whose turn it is"):
    val (_, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven)
    alice.inbox.last shouldBe ActionRequest("Alice", canDoubleDown = false, canSplit = false)
    lastTable(alice).currentTurn shouldBe Some("Alice")

  test("hit gives one more card and the player is asked again"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Two)
    dealt.onMessage(alice, Hit)
    lastTable(alice).players.head.cards.map(_.value) shouldBe List(Value.Ten, Value.Six, Value.Two)
    alice.inbox.last shouldBe ActionRequest("Alice", canDoubleDown = false, canSplit = false)

  test("stand ends the player's turn"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Two)
    dealt.onMessage(alice, Stand)
    aliceState(alice) shouldBe PlayerState.Standing
    tableAtEndOfHand(alice).currentTurn shouldBe None

  test("a player who busts ends the turn"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.King)
    dealt.onMessage(alice, Hit)
    aliceState(alice) shouldBe PlayerState.Busted
    tableAtEndOfHand(alice).currentTurn shouldBe None

  test("a player who reaches 21 stands automatically"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Five)
    dealt.onMessage(alice, Hit)
    aliceState(alice) shouldBe PlayerState.Standing
    tableAtEndOfHand(alice).currentTurn shouldBe None

  test("a player with an initial blackjack is paid at once and does not play"):
    val (_, alice) = aliceDealt(Value.Ace, Value.Nine, Value.King, Value.Seven, Value.Two)
    aliceState(alice) shouldBe PlayerState.Blackjack
    handResults(alice) shouldBe List(("Alice", DefaultBalance - 10 + 25))
    alice.inbox.exists(_.isInstanceOf[ActionRequest]) shouldBe false

  test("double down and split are not available yet"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven)
    dealt.onMessage(alice, DoubleDown)
    alice.inbox.last shouldBe ErrorMessage(ActionNotAllowed)

  /** Alice and Bob seated, both bet 10, cards dealt: Alice 10+6, Bob 9+7, dealer 8 and a hidden 5. */
  private def aliceAndBobDealt(): (TableHandler, FakeConnection, FakeConnection) =
    // deal order: Alice, Bob, dealer, Alice, Bob, dealer (face down)
    val dealt = tableWithDeck(List(Value.Ten, Value.Nine, Value.Eight, Value.Six, Value.Seven, Value.Five).map(card)*)
    val alice = join("Alice", dealt)
    val bob   = join("Bob", dealt)
    dealt.onMessage(alice, PlaceBet(10))
    dealt.onMessage(bob, PlaceBet(10))
    (dealt, alice, bob)

  test("only the player whose turn it is can act"):
    val (dealt, alice, bob) = aliceAndBobDealt()
    dealt.onMessage(bob, Hit)
    bob.inbox.last shouldBe ErrorMessage(NotYourTurn)
    lastTable(alice).players.find(_.name == "Bob").get.cards.size shouldBe 2
    lastTable(alice).currentTurn shouldBe Some("Alice") // nothing changed

  test("when a player ends the turn the next one is asked to act"):
    val (dealt, alice, bob) = aliceAndBobDealt()
    dealt.onMessage(alice, Stand)
    bob.inbox.last shouldBe ActionRequest("Bob", canDoubleDown = false, canSplit = false)
    lastTable(alice).currentTurn shouldBe Some("Bob")

  test("if the player whose turn it is leaves, the turn passes to the next one"):
    val (dealt, alice, bob) = aliceAndBobDealt()
    dealt.onMessage(alice, LeaveTable)
    bob.inbox.last shouldBe ActionRequest("Bob", canDoubleDown = false, canSplit = false)

  test("after the last player the dealer reveals the hidden card and draws up to 17, and everyone sees it"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Two)
    dealt.onMessage(alice, Stand)
    val dealer = tableAtEndOfHand(alice).dealer
    dealer.hiddenCards shouldBe 0
    dealer.visibleCards.map(_.value) shouldBe List(Value.Nine, Value.Seven, Value.Two) // 18

  test("the dealer does not reveal the hidden card while a player is still playing"):
    val (_, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Two)
    lastTable(alice).dealer.hiddenCards shouldBe 1

  test("a player who beats the dealer is paid 1:1 and the results are sent to the table"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Queen, Value.Seven, Value.Two) // 20
    dealt.onMessage(alice, Stand)
    handResults(alice) shouldBe List(("Alice", DefaultBalance - 10 + 20))

  test("a player who loses against the dealer loses the bet"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Two) // 16
    dealt.onMessage(alice, Stand)
    handResults(alice) shouldBe List(("Alice", DefaultBalance - 10))

  test("on equal scores the bet is returned"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Eight, Value.Nine) // 18
    dealt.onMessage(alice, Stand)
    handResults(alice) shouldBe List(("Alice", DefaultBalance))

  test("when the dealer busts the players still in the game win"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.King)
    dealt.onMessage(alice, Stand)
    tableAtEndOfHand(alice).dealer.visibleCards.size shouldBe 3
    handResults(alice) shouldBe List(("Alice", DefaultBalance - 10 + 20))

  test("a busted player loses even if the dealer busts too"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.King, Value.Queen)
    dealt.onMessage(alice, Hit) // 26
    handResults(alice) shouldBe List(("Alice", DefaultBalance - 10))

  test("with more players every balance is settled against the same dealer's hand"):
    val dealt = tableWithDeck(List(Value.Ten, Value.Nine, Value.Eight, Value.Six, Value.Seven, Value.Five,
      Value.Five, Value.Four).map(card)*)
    val alice = join("Alice", dealt)
    val bob   = join("Bob", dealt)
    dealt.onMessage(alice, PlaceBet(10))
    dealt.onMessage(bob, PlaceBet(10))
    dealt.onMessage(alice, Hit)
    dealt.onMessage(bob, Stand)
    val expected = List(("Alice", DefaultBalance - 10 + 20), ("Bob", DefaultBalance - 10))
    handResults(alice) shouldBe expected
    handResults(bob) shouldBe expected

  test("when the hand is over the table is cleaned and everybody is asked for a new bet"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Two)
    dealt.onMessage(alice, Stand)
    alice.inbox.last shouldBe BetRequest(DefaultBalance - 10, Game.MinGameBet)
    val view = lastTable(alice)
    view.players.head.cards.isEmpty shouldBe true
    view.players.head.state shouldBe PlayerState.Active
    view.players.head.bet shouldBe None
    view.dealer.visibleCards.isEmpty shouldBe true

  test("a second hand can be played right after the first one"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Two,
      Value.Ten, Value.Nine, Value.Queen, Value.Seven, Value.Two)
    dealt.onMessage(alice, Stand)
    dealt.onMessage(alice, PlaceBet(10))
    dealt.onMessage(alice, Stand) // 20 against 18
    alice.inbox.collect { case HandResults(b) => b } shouldBe
      List(List(("Alice", DefaultBalance - 10)), List(("Alice", DefaultBalance - 10 + 10)))

  test("a player left without money to bet is told the game is over for them and leaves the table"):
    val dealt = tableWithDeck(List(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Two).map(card)*)
    val alice = join("Alice", dealt)
    dealt.onMessage(alice, PlaceBet(100)) // the whole balance
    dealt.onMessage(alice, Stand)
    alice.inbox.last shouldBe GameOver(List(("Alice", 0.0)))
    join("Alice", dealt).inbox.head shouldBe JoinAccepted("Alice", DefaultBalance) // the seat is free again

  test("a broke player leaves while the others go on with a new hand"):
    val dealt = tableWithDeck(List(Value.Ten, Value.Nine, Value.Eight, Value.Six, Value.Seven, Value.Five,
      Value.Five, Value.Four).map(card)*)
    val alice = join("Alice", dealt)
    val bob   = join("Bob", dealt)
    dealt.onMessage(alice, PlaceBet(10))
    dealt.onMessage(bob, PlaceBet(100))
    dealt.onMessage(alice, Hit)
    dealt.onMessage(bob, Stand) // Bob loses everything
    bob.inbox.last shouldBe GameOver(List(("Bob", 0.0)))
    alice.inbox.last shouldBe BetRequest(DefaultBalance + 10, Game.MinGameBet)
    lastTable(alice).players.map(_.name) shouldBe List("Alice")

  test("a player who sits down during a hand plays from the next one"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Two)
    val bob = join("Bob", dealt) // the hand of Alice is in progress
    bob.inbox.exists(_.isInstanceOf[BetRequest]) shouldBe false
    dealt.onMessage(alice, Stand)
    bob.inbox.last shouldBe BetRequest(DefaultBalance, Game.MinGameBet)
    handResults(bob) shouldBe List(("Alice", DefaultBalance - 10), ("Bob", DefaultBalance))

  test("when the cut card comes out the game is over for everybody and the table restarts"):
    // the cut card appears among the dealer's cards, it is skipped, as in the local game
    val dealt = TableHandler(newGame = () => Game(List.empty,
      Deck.testDeck(card(Value.Ten), card(Value.Nine), card(Value.Six), card(Value.Seven), CutCard, card(Value.Two))))
    val alice = join("Alice", dealt)
    dealt.onMessage(alice, PlaceBet(10))
    dealt.onMessage(alice, Stand)
    alice.inbox.last shouldBe GameOver(List(("Alice", DefaultBalance - 10)))
    join("Alice", dealt).inbox.head shouldBe JoinAccepted("Alice", DefaultBalance) // a new game for new players

  /** Every table snapshot received by the client, in order. */
  private def updates(connection: FakeConnection): List[TableView] =
    connection.inbox.toList.collect { case TableUpdate(view) => view }

  /** The snapshots received from the given version on. */
  private def updatesFrom(version: Long)(connection: FakeConnection): List[TableView] =
    updates(connection).filter(_.version >= version)

  /** Alice, Bob and Carol seated, all bet 10, cards dealt and the hand played to the end:
   * Alice 10+6 stands, Bob has a blackjack (the dealer shows a nine: paid at once, he does not play),
   * Carol 9+7 stands; the dealer 9+7 draws a two (18). */
  private def threePlayersHand(): (TableHandler, FakeConnection, FakeConnection, FakeConnection) =
    // deal order: Alice, Bob, Carol, dealer, Alice, Bob, Carol, dealer (face down), then the dealer's draw
    val dealt = tableWithDeck(List(Value.Ten, Value.Ace, Value.Nine, Value.Nine,
      Value.Six, Value.King, Value.Seven, Value.Seven, Value.Two).map(card)*)
    val alice = join("Alice", dealt)
    val bob   = join("Bob", dealt)
    val carol = join("Carol", dealt)
    List(alice, bob, carol).foreach(dealt.onMessage(_, PlaceBet(10)))
    dealt.onMessage(alice, Stand)
    dealt.onMessage(carol, Stand)
    (dealt, alice, bob, carol)

  test("every client receives the same snapshots of the table"):
    val (_, alice, bob, carol) = threePlayersHand()
    val fromCarolJoin = updates(carol).head.version // from here on all three are seated
    val seenByAlice = updatesFrom(fromCarolJoin)(alice)
    (seenByAlice.size > 5) shouldBe true
    updatesFrom(fromCarolJoin)(bob) shouldBe seenByAlice
    updatesFrom(fromCarolJoin)(carol) shouldBe seenByAlice

  test("the snapshots are numbered one after the other"):
    val (_, alice, _, _) = threePlayersHand()
    val versions = updates(alice).map(_.version)
    versions shouldBe (versions.head to versions.last).toList

  test("the turns follow the seating order and skip who does not have to play"):
    val (_, alice, bob, carol) = threePlayersHand()
    val turns = updates(bob).map(_.currentTurn).filter(_.isDefined).distinct
    turns shouldBe List(Some("Alice"), Some("Carol"))
    alice.inbox.count(_.isInstanceOf[ActionRequest]) shouldBe 1
    bob.inbox.count(_.isInstanceOf[ActionRequest]) shouldBe 0
    carol.inbox.count(_.isInstanceOf[ActionRequest]) shouldBe 1

  test("every client gets the results of all the players"):
    val (_, alice, bob, carol) = threePlayersHand()
    val expected = List(("Alice", DefaultBalance - 10), ("Bob", DefaultBalance - 10 + 25), ("Carol", DefaultBalance - 10))
    List(alice, bob, carol).foreach(client => handResults(client) shouldBe expected)

  test("a player who sits down during a hand immediately sees the current state of the table"):
    val (dealt, alice, _) = aliceAndBobDealt()
    val dave = join("Dave", dealt)
    val seenByDave = updates(dave).head
    seenByDave shouldBe lastTable(alice) // the same snapshot, with the same version
    seenByDave.currentTurn shouldBe Some("Alice")
    seenByDave.dealer.hiddenCards shouldBe 1
