package server

import utils.TestExports.*
import utils.ModelExports.{CutCard, Deck, Game, PlayerState, StandardCard, Suit, Value}
import protocol.{Decision, ServerMessage, TableView}
import protocol.ClientMessage.*
import protocol.ServerMessage.*
import server.TableHandler.*
import java.time.Instant
import java.util.UUID
import scala.collection.mutable.ListBuffer

/** The table logic is tested without network nor threads: it is called directly,
 * as the single writer thread would do, with fake connections that record what they receive. */
class TableHandlerTest extends AnyFunSuite with BeforeAndAfterEach:

  private final class FakeConnection(val id: String) extends Connection:
    val inbox = ListBuffer[ServerMessage]()
    def send(message: ServerMessage): Unit = inbox += message

  /** A cashier that gives each player the balance set by the test (100 by default) and records what the
   * table gives back when they leave. */
  private final class RecordingCashier extends Cashier:
    var balances = Map.empty[String, Double]
    var refusals = Map.empty[String, String]
    val returned = ListBuffer[(String, Double)]()
    def sitDown(username: String): Either[String, Double] =
      refusals.get(username).toLeft(balances.getOrElse(username, DefaultBalance))
    def standUp(username: String, balance: Double): Unit = returned += username -> balance

  var table: TableHandler = _
  private var cashier: RecordingCashier = _

  /** The clock of the tables built by [[tableWithDeck]]: it only moves when a test moves it. */
  private val startTime = Instant.parse("2026-01-01T00:00:00Z")
  private var now = startTime

  override def beforeEach(): Unit =
    now = startTime
    cashier = RecordingCashier()
    table = TableHandler(cashier = cashier)

  /** A table whose deck deals exactly the given cards, in this order. */
  private def tableWithDeck(cards: StandardCard*): TableHandler =
    TableHandler(cashier = cashier, newGame = () => Game(List.empty, Deck.testDeck(cards*)), clock = () => now)

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

  test("a disconnection while nobody is playing frees the seat at once"):
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
    table = tableWithDeck(card(Value.Ten), card(Value.Nine), card(Value.Six), card(Value.Seven))
    val alice = join("Alice")
    table.onMessage(alice, PlaceBet(3))    // not a multiple of the minimum bet
    alice.inbox.last shouldBe ErrorMessage(InvalidBet)
    table.onMessage(alice, PlaceBet(1000)) // more than the balance
    alice.inbox.last shouldBe ErrorMessage(InvalidBet)
    table.onMessage(alice, PlaceBet(10))

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
    // a known deck: with a random one a blackjack could end the hand at once
    table = tableWithDeck(List(Value.Ten, Value.Nine, Value.Eight, Value.Six, Value.Seven, Value.Five).map(card)*)
    val alice = join("Alice")
    val bob   = join("Bob")
    table.onMessage(alice, PlaceBet(10))
    lastTable(alice).dealer.visibleCards.isEmpty shouldBe true
    table.onMessage(bob, PlaceBet(10))
    lastTable(alice).dealer.visibleCards.size shouldBe 1

  test("a player who leaves during the betting is not waited for"):
    table = tableWithDeck(card(Value.Ten), card(Value.Nine), card(Value.Six), card(Value.Seven))
    val alice = join("Alice")
    val bob   = join("Bob")
    table.onMessage(alice, PlaceBet(10))
    table.onMessage(bob, LeaveTable)
    lastTable(alice).dealer.visibleCards.size shouldBe 1

  test("bets are refused once the cards are dealt"):
    table = tableWithDeck(card(Value.Ten), card(Value.Nine), card(Value.Six), card(Value.Seven))
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
  private def aliceDealt(cards: Value*): (TableHandler, FakeConnection) = aliceBetting(10, cards*)

  /** Same as [[aliceDealt]], but Alice bets `bet`. */
  private def aliceBetting(bet: Int, cards: Value*): (TableHandler, FakeConnection) =
    val dealt = tableWithDeck(cards.map(card)*)
    val alice = join("Alice", dealt)
    dealt.onMessage(alice, PlaceBet(bet))
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
    alice.inbox.last shouldBe ActionRequest("Alice", canDoubleDown = true, canSplit = false)
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

  test("a split is refused when the hand is not a pair"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven) // 10 + 6
    dealt.onMessage(alice, Split)
    alice.inbox.last shouldBe ErrorMessage(ActionNotAllowed)

  /** Alice and Bob seated, both bet 10, cards dealt: Alice 10+6, Bob 9+7, dealer 8 and a hidden 5. */
  private def aliceAndBobDealt(): (TableHandler, FakeConnection, FakeConnection) =
    aliceAndBobWith(Value.Ten, Value.Nine, Value.Eight, Value.Six, Value.Seven, Value.Five)

  /** Alice and Bob seated, both bet 10, the cards dealt from `cards`
   * (deal order: Alice, Bob, dealer, Alice, Bob, dealer face down, then the cards drawn later). */
  private def aliceAndBobWith(cards: Value*): (TableHandler, FakeConnection, FakeConnection) =
    val dealt = tableWithDeck(cards.map(card)*)
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
    bob.inbox.last shouldBe ActionRequest("Bob", canDoubleDown = true, canSplit = false)
    lastTable(alice).currentTurn shouldBe Some("Bob")

  test("if the player whose turn it is leaves, the turn passes to the next one"):
    val (dealt, alice, bob) = aliceAndBobDealt()
    dealt.onMessage(alice, LeaveTable)
    bob.inbox.last shouldBe ActionRequest("Bob", canDoubleDown = true, canSplit = false)

  test("after the last player the dealer reveals the hidden card and draws up to 17, and everyone sees it"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Two)
    dealt.onMessage(alice, Stand)
    val dealer = tableAtEndOfHand(alice).dealer
    dealer.hiddenCards shouldBe 0
    dealer.visibleCards.map(_.value) shouldBe List(Value.Nine, Value.Seven, Value.Two) // 18

  test("the face-down card is revealed to everyone before the dealer draws"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Two)
    dealt.onMessage(alice, Stand)
    val revealed = updates(alice).filter(view => view.dealer.hiddenCards == 0 && view.dealer.visibleCards.nonEmpty)
    revealed.head.dealer.visibleCards.map(_.value) shouldBe List(Value.Nine, Value.Seven) // the reveal alone
    revealed.head.players.head.balance shouldBe DefaultBalance - 10

  test("the dealer draws one card at a time, and everyone sees each card before the next one"):
    // dealer 4+5 = 9, then a two (11), a three (14) and a four (18)
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Four, Value.Eight, Value.Five, Value.Two, Value.Three, Value.Four)
    dealt.onMessage(alice, Stand)
    val dealerHands = updates(alice).map(_.dealer).filter(_.hiddenCards == 0).map(_.visibleCards.size).distinct
    dealerHands.filter(_ >= 2) shouldBe List(2, 3, 4, 5)

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

  // ---- the seats released by a voluntary exit or by the end of the game are free at once ----

  /** Fills every seat of `at`: Player1, Player2, ... */
  private def fillTable(at: TableHandler = table): List[FakeConnection] =
    (1 to Game.MaxPlayersNum).toList.map(i => join(s"Player$i", at))

  test("an empty table has every seat free and each player who sits down takes one"):
    table.freeSeats shouldBe Game.MaxPlayersNum
    join("Alice")
    table.freeSeats shouldBe Game.MaxPlayersNum - 1

  test("a player who is refused does not take a seat"):
    join("Alice")
    join("Alice") // name already taken
    table.freeSeats shouldBe Game.MaxPlayersNum - 1

  test("a player who leaves a full table frees the seat and a newcomer can take it at once"):
    val players = fillTable()
    table.freeSeats shouldBe 0
    table.onMessage(players.head, LeaveTable)
    table.freeSeats shouldBe 1
    join("Late").inbox.head shouldBe JoinAccepted("Late", DefaultBalance)
    table.freeSeats shouldBe 0

  test("a player who leaves in the middle of a hand frees the seat without waiting for the end of the hand"):
    val (dealt, _, bob) = aliceAndBobDealt() // it is Alice's turn
    dealt.freeSeats shouldBe Game.MaxPlayersNum - 2
    dealt.onMessage(bob, LeaveTable)
    dealt.freeSeats shouldBe Game.MaxPlayersNum - 1
    join("Carol", dealt).inbox.head shouldBe JoinAccepted("Carol", DefaultBalance)

  test("a player left without money frees the seat"):
    val dealt = tableWithDeck(List(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Two).map(card)*)
    val alice = join("Alice", dealt)
    dealt.onMessage(alice, PlaceBet(100)) // the whole balance
    dealt.onMessage(alice, Stand)
    dealt.freeSeats shouldBe Game.MaxPlayersNum

  test("when the cut card comes out every player is told the game is over and every seat is free again"):
    // Alice 10+6 and Bob 9+7 stand; the dealer has 8+5, skips the cut card and draws a two and a three (18)
    val newGame = () => Game(List.empty, Deck.testDeck(card(Value.Ten), card(Value.Nine), card(Value.Eight),
      card(Value.Six), card(Value.Seven), card(Value.Five), CutCard, card(Value.Two), card(Value.Three), card(Value.Five)))
    val dealt = TableHandler(newGame = newGame)
    val alice = join("Alice", dealt)
    val bob   = join("Bob", dealt)
    List(alice, bob).foreach(dealt.onMessage(_, PlaceBet(10)))
    dealt.onMessage(alice, Stand)
    dealt.onMessage(bob, Stand)
    alice.inbox.last shouldBe GameOver(List(("Alice", DefaultBalance - 10)))
    bob.inbox.last shouldBe GameOver(List(("Bob", DefaultBalance - 10)))
    dealt.freeSeats shouldBe Game.MaxPlayersNum
    fillTable(dealt).foreach(_.inbox.head shouldBe a[JoinAccepted]) // the whole table is available again

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

  test("double down doubles the bet, draws one card and ends the turn"):
    // Alice 5+6 doubles and draws a ten (21); the dealer has 17, so Alice wins the doubled bet
    val (dealt, alice) = aliceDealt(Value.Five, Value.Ten, Value.Six, Value.Seven, Value.Ten)
    dealt.onMessage(alice, DoubleDown)
    val hand = tableAtEndOfHand(alice).players.head
    hand.cards.map(_.value) shouldBe List(Value.Five, Value.Six, Value.Ten)
    hand.bet shouldBe Some(20)
    hand.state shouldBe PlayerState.Standing
    handResults(alice) shouldBe List(("Alice", DefaultBalance - 20 + 40))

  test("a doubled hand that busts loses the doubled bet"):
    // Alice 10+6 doubles and draws a king (26)
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Eight, Value.King)
    dealt.onMessage(alice, DoubleDown)
    aliceState(alice) shouldBe PlayerState.Busted
    handResults(alice) shouldBe List(("Alice", DefaultBalance - 20))

  test("double down is refused after the first card"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Two)
    dealt.onMessage(alice, Hit)
    dealt.onMessage(alice, DoubleDown)
    alice.inbox.last shouldBe ErrorMessage(ActionNotAllowed)

  test("double down is neither offered nor accepted when the balance cannot cover the doubled bet"):
    val (dealt, alice) = aliceBetting(DefaultBalance.toInt, Value.Ten, Value.Nine, Value.Six, Value.Seven)
    alice.inbox.last shouldBe ActionRequest("Alice", canDoubleDown = false, canSplit = false)
    dealt.onMessage(alice, DoubleDown)
    alice.inbox.last shouldBe ErrorMessage(ActionNotAllowed)

  /** Alice has a pair of eights and the dealer shows a nine; then come the two cards of the split (a three and
   * a two) and a ten for the dealer, who has 9+7=16 and busts. */
  private val splitDeck = List(Value.Eight, Value.Nine, Value.Eight, Value.Seven, Value.Three, Value.Two, Value.Ten)

  test("a pair can be split: the player is offered the split"):
    val (_, alice) = aliceDealt(splitDeck*)
    alice.inbox.last shouldBe ActionRequest("Alice", canDoubleDown = true, canSplit = true)

  test("a split makes two hands, each with its own bet and a new card, and the player plays the first again"):
    val (dealt, alice) = aliceDealt(splitDeck*)
    dealt.onMessage(alice, Split)
    val table = lastTable(alice)
    table.players.map(_.name) shouldBe List("Alice", "Alice_split1")
    table.players.map(_.cards.map(_.value)) shouldBe List(List(Value.Eight, Value.Three), List(Value.Eight, Value.Two))
    table.players.map(_.bet) shouldBe List(Some(10), Some(10))
    table.currentTurn shouldBe Some("Alice")
    alice.inbox.last shouldBe ActionRequest("Alice", canDoubleDown = true, canSplit = false)

  test("after the first hand the turn passes to the split hand, and its requests reach the same client"):
    val (dealt, alice) = aliceDealt(splitDeck*)
    dealt.onMessage(alice, Split)
    dealt.onMessage(alice, Stand)
    lastTable(alice).currentTurn shouldBe Some("Alice_split1")
    alice.inbox.last shouldBe ActionRequest("Alice_split1", canDoubleDown = true, canSplit = false)

  test("both hands are paid at the end and the money goes back to the player"):
    val (dealt, alice) = aliceDealt(splitDeck*)
    dealt.onMessage(alice, Split)
    dealt.onMessage(alice, Stand)
    dealt.onMessage(alice, Stand)
    tableAtEndOfHand(alice).players.map(_.state) shouldBe List(PlayerState.Standing, PlayerState.Standing)
    handResults(alice) shouldBe List(("Alice", DefaultBalance - 20 + 40)) // the dealer busts: both hands win 20

  test("a player can split with all the money left and the hands are still paid"):
    val (dealt, alice) = aliceBetting(50, splitDeck*) // 50 for the bet and 50 for the split: the balance is 0
    dealt.onMessage(alice, Split)
    dealt.onMessage(alice, Stand)
    alice.inbox.last shouldBe ActionRequest("Alice_split1", canDoubleDown = false, canSplit = false)
    dealt.onMessage(alice, Stand)
    handResults(alice) shouldBe List(("Alice", DefaultBalance - 100 + 200))

  test("a blackjack after a split is paid at once and the other hand keeps playing"):
    // Alice A+A splits: the first hand gets a king (blackjack), the new one a nine; the dealer 6+5 draws a ten (21)
    val (dealt, alice) = aliceDealt(Value.Ace, Value.Six, Value.Ace, Value.Five, Value.King, Value.Nine, Value.Ten)
    dealt.onMessage(alice, Split)
    alice.inbox.last shouldBe ActionRequest("Alice_split1", canDoubleDown = false, canSplit = false)
    dealt.onMessage(alice, Stand)
    // 100 - 10 - 10 (the two bets) + 25 (the blackjack); the second hand loses and has no money of its own
    handResults(alice) shouldBe List(("Alice", DefaultBalance - 20 + 25))

  test("a player who leaves after a split takes all the hands away"):
    // Alice 8+8, Bob 9+7, dealer 10 and a hidden 6; the split draws a three and a two
    val (dealt, alice, bob) = aliceAndBobWith(Value.Eight, Value.Nine, Value.Ten, Value.Eight, Value.Seven,
      Value.Six, Value.Three, Value.Two)
    dealt.onMessage(alice, Split)
    dealt.onMessage(alice, LeaveTable)
    lastTable(bob).players.map(_.name) shouldBe List("Bob")
    bob.inbox.last shouldBe ActionRequest("Bob", canDoubleDown = true, canSplit = false)

  test("players whose names contain each other do not exchange money"):
    val dealt = tableWithDeck(List(Value.Ten, Value.Nine, Value.Eight, Value.Six, Value.Seven, Value.Five).map(card)*)
    val ann  = join("Ann", dealt)
    val anna = join("Anna", dealt)
    List(ann, anna).foreach(dealt.onMessage(_, PlaceBet(10)))
    dealt.onMessage(ann, Stand)
    lastTable(ann).players.map(player => (player.name, player.balance)) shouldBe
      List(("Ann", DefaultBalance - 10), ("Anna", DefaultBalance - 10))

  /** Alice and Bob seated, both bet 10; the dealer shows an ace and has a hidden six. */
  private def aliceAndBobFacingAce(): (TableHandler, FakeConnection, FakeConnection) =
    aliceAndBobWith(Value.Ten, Value.Nine, Value.Ace, Value.Six, Value.Seven, Value.Six)

  test("when the dealer shows an ace the player is offered the insurance before the turns begin"):
    val (_, alice) = aliceDealt(Value.Ten, Value.Ace, Value.Six, Value.King)
    alice.inbox.last shouldBe InsuranceOffer
    lastTable(alice).dealer.visibleCards.map(_.value) shouldBe List(Value.Ace) // the client sees the ace it is asked about
    alice.inbox.exists(_.isInstanceOf[ActionRequest]) shouldBe false

  test("the insurance is not offered when the dealer does not show an ace"):
    val (_, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven)
    alice.inbox.contains(InsuranceOffer) shouldBe false

  test("the turns begin only when every player has answered the insurance offer"):
    val (dealt, alice, bob) = aliceAndBobFacingAce()
    List(alice, bob).foreach(_.inbox.last shouldBe InsuranceOffer)
    dealt.onMessage(alice, AnswerInsurance(true))
    alice.inbox.exists(_.isInstanceOf[ActionRequest]) shouldBe false // Bob has not answered yet
    dealt.onMessage(bob, AnswerInsurance(false))
    alice.inbox.last shouldBe ActionRequest("Alice", canDoubleDown = true, canSplit = false)

  test("an accepted insurance costs half of the bet, a declined one costs nothing"):
    val (dealt, alice, bob) = aliceAndBobFacingAce()
    dealt.onMessage(alice, AnswerInsurance(true))
    dealt.onMessage(bob, AnswerInsurance(false))
    val table = lastTable(alice)
    table.players.map(_.bet) shouldBe List(Some(15), Some(10)) // Alice: 10 + 5 of insurance
    table.players.map(_.balance) shouldBe List(DefaultBalance - 15, DefaultBalance - 10)

  test("with the dealer's blackjack the insurance is paid, as in the local game"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Ace, Value.Six, Value.King) // the dealer has a blackjack
    dealt.onMessage(alice, AnswerInsurance(true))
    dealt.onMessage(alice, Stand)
    // 100 - 10 (the bet, lost) - 5 (the insurance) + 10 (paid by the insurance)
    handResults(alice) shouldBe List(("Alice", DefaultBalance - 10 - 5 + 10))

  test("an insurance answer is refused when no insurance was offered"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven)
    dealt.onMessage(alice, AnswerInsurance(true))
    alice.inbox.last shouldBe ErrorMessage(NoInsuranceOffered)

  test("a player cannot answer the insurance offer twice"):
    val (dealt, alice, _) = aliceAndBobFacingAce()
    dealt.onMessage(alice, AnswerInsurance(true))
    dealt.onMessage(alice, AnswerInsurance(true))
    alice.inbox.last shouldBe ErrorMessage(NoInsuranceOffered)
    lastTable(alice).players.head.bet shouldBe Some(10) // the insurance is applied once, when everybody has answered

  test("the table does not wait for the insurance answer of a player who leaves"):
    val (dealt, alice, bob) = aliceAndBobFacingAce()
    dealt.onMessage(alice, AnswerInsurance(true))
    dealt.onMessage(bob, LeaveTable)
    alice.inbox.last shouldBe ActionRequest("Alice", canDoubleDown = true, canSplit = false)
    lastTable(alice).players.map(_.bet) shouldBe List(Some(15))

  // ---- disconnection in the middle of a hand, and decisions that do not arrive ----

  /** Alice 10+6 and Bob 9+7 against a dealer 8+5 who draws a ten and busts: if both stand, both win 20. */
  private def aliceAndBobAgainstBustingDealer(): (TableHandler, FakeConnection, FakeConnection) =
    aliceAndBobWith(Value.Ten, Value.Nine, Value.Eight, Value.Six, Value.Seven, Value.Five, Value.Ten)

  test("a player who disconnects on their turn stands automatically and the next player is asked"):
    val (dealt, alice, bob) = aliceAndBobAgainstBustingDealer()
    dealt.onDisconnected(alice)
    bob.inbox.last shouldBe ActionRequest("Bob", canDoubleDown = true, canSplit = false)
    lastTable(bob).currentTurn shouldBe Some("Bob")
    lastTable(bob).players.map(_.state) shouldBe List(PlayerState.Standing, PlayerState.Active)
    lastTable(bob).players.map(_.name) shouldBe List("Alice", "Bob") // the seat is kept until the end of the hand

  test("the bet of a player who disconnected is settled like the others and the seat is freed at the end of the hand"):
    val (dealt, alice, bob) = aliceAndBobAgainstBustingDealer()
    dealt.onDisconnected(alice)
    dealt.onMessage(bob, Stand)
    handResults(bob) shouldBe List(("Alice", DefaultBalance - 10 + 20), ("Bob", DefaultBalance - 10 + 20))
    lastTable(bob).players.map(_.name) shouldBe List("Bob") // the next hand starts without Alice
    bob.inbox.last shouldBe BetRequest(DefaultBalance - 10 + 20, Game.MinGameBet)

  test("nothing is sent any more to a client that has disconnected"):
    val (dealt, alice, bob) = aliceAndBobAgainstBustingDealer()
    dealt.onDisconnected(alice)
    val messagesBefore = alice.inbox.size
    dealt.onMessage(bob, Stand)
    alice.inbox.size shouldBe messagesBefore

  test("the hands of a player who disconnected stand when their turn comes, without asking"):
    val (dealt, alice, bob) = aliceAndBobAgainstBustingDealer()
    dealt.onDisconnected(bob) // it is Alice's turn: Bob's hand has to wait for its turn
    dealt.onMessage(alice, Stand)
    bob.inbox.exists(_.isInstanceOf[ActionRequest]) shouldBe false
    handResults(alice) shouldBe List(("Alice", DefaultBalance - 10 + 20), ("Bob", DefaultBalance - 10 + 20))

  test("the table plays all the hands of a player who disconnects after a split"):
    // Alice 8+8, Bob 9+7, dealer 9+7; Alice splits (a three and a two) and disconnects: her hands are worth 11 and 10,
    // so each gets one more card (a nine and a seven) and stands; the dealer draws a ten and busts
    val (dealt, alice, bob) = aliceAndBobWith(Value.Eight, Value.Nine, Value.Nine, Value.Eight, Value.Seven,
      Value.Seven, Value.Three, Value.Two, Value.Nine, Value.Seven, Value.Ten)
    dealt.onMessage(alice, Split)
    dealt.onDisconnected(alice)
    bob.inbox.last shouldBe ActionRequest("Bob", canDoubleDown = true, canSplit = false)
    dealt.onMessage(bob, Stand)
    handResults(bob) shouldBe List(("Alice", DefaultBalance - 20 + 40), ("Bob", DefaultBalance - 10 + 20))

  test("a player who disconnects during the insurance offer counts as declining and nobody waits"):
    val (dealt, alice, bob) = aliceAndBobFacingAce()
    dealt.onMessage(alice, AnswerInsurance(true))
    dealt.onDisconnected(bob)
    alice.inbox.last shouldBe ActionRequest("Alice", canDoubleDown = true, canSplit = false)
    lastTable(alice).players.map(_.bet) shouldBe List(Some(15), Some(10)) // Bob is still seated, without insurance

  /** Moves the clock of the tables built by [[tableWithDeck]] and lets the table notice it. */
  private def passTime(at: TableHandler, seconds: Long): Unit =
    now = now.plusSeconds(seconds)
    at.onTick()

  test("a player who does not act in time stands automatically and the next player is asked"):
    val (dealt, alice, bob) = aliceAndBobAgainstBustingDealer()
    passTime(dealt, DefaultDecisionTimeout.toSeconds + 1)
    bob.inbox.last shouldBe ActionRequest("Bob", canDoubleDown = true, canSplit = false)
    lastTable(bob).players.map(_.state) shouldBe List(PlayerState.Standing, PlayerState.Active)
    lastTable(alice).players.map(_.name) shouldBe List("Alice", "Bob") // Alice is late, not gone: she keeps her seat

  test("the table does not decide for a player before the time is up"):
    val (dealt, _, bob) = aliceAndBobAgainstBustingDealer()
    passTime(dealt, DefaultDecisionTimeout.toSeconds - 1)
    lastTable(bob).currentTurn shouldBe Some("Alice")
    bob.inbox.exists(_.isInstanceOf[ActionRequest]) shouldBe false

  test("every action gives the player a new time to decide"):
    val (dealt, alice, bob) = aliceAndBobWith(Value.Ten, Value.Nine, Value.Eight, Value.Six, Value.Seven, Value.Five, Value.Two)
    passTime(dealt, 20)
    dealt.onMessage(alice, Hit)              // at 20 s: the new time runs until 50 s
    passTime(dealt, 20)                      // 40 s: only 20 s after her last action
    lastTable(bob).currentTurn shouldBe Some("Alice")
    passTime(dealt, 11)                      // 51 s: more than 30 s after her last action
    lastTable(bob).currentTurn shouldBe Some("Bob")

  test("a player who does not answer the insurance offer in time declines it"):
    val (dealt, alice, _) = aliceAndBobFacingAce()
    dealt.onMessage(alice, AnswerInsurance(true))
    passTime(dealt, DefaultDecisionTimeout.toSeconds + 1)
    alice.inbox.last shouldBe ActionRequest("Alice", canDoubleDown = true, canSplit = false)
    lastTable(alice).players.map(_.bet) shouldBe List(Some(15), Some(10)) // only Alice bought it

  test("a late tick after the hand is over changes nothing until the time to bet is up"):
    val (dealt, alice, bob) = aliceAndBobAgainstBustingDealer()
    dealt.onMessage(alice, Stand)
    dealt.onMessage(bob, Stand) // the hand is over: the bets for the next one are requested with a new time
    val messagesBefore = alice.inbox.size
    passTime(dealt,  DefaultDecisionTimeout.toSeconds - 1) // the deadline of the old turn is long gone, this one is not)
    alice.inbox.size shouldBe messagesBefore

  // ---- the players who are late are told that time is up, and a hand never waits for a bet ----

  /** The decisions that the table has declared timed out to this client, in order. */
  private def timeouts(connection: FakeConnection): List[Decision] =
    connection.inbox.collect { case DecisionTimedOut(decision) => decision }.toList

  /** Alice and Bob seated and asked to bet; only Alice bets. The deck is for a hand of Alice alone
  * (deal order: Alice, dealer, Alice, dealer face down, then the cards drawn later). */
  private def aliceBetsBobIsSilent(cards: Value*): (TableHandler, FakeConnection, FakeConnection) =
    val dealt = tableWithDeck(cards.map(card) *)
    val alice = join("Alice", dealt)
    val bob = join("Bob", dealt)
    dealt.onMessage(alice, PlaceBet(10))
    (dealt, alice, bob)

  /** Alice 10+6 against a dealer 9+7 who draws a ten and busts: if Alice stands she wins 20. */
  private val aliceHandOnly = List(Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Ten)

  test("a player who does not act in time is told that the time is up"):
    val (dealt, alice, bob) = aliceAndBobAgainstBustingDealer()
    passTime(dealt, DefaultDecisionTimeout.toSeconds + 1)
    timeouts(alice) shouldBe List(Decision.Action)
    timeouts(bob) shouldBe List.empty // Bob is not late: it is only his turn now

  test("a player who does not answer the insurance offer in time is told that the time is up"):
    val (dealt, alice, bob) = aliceAndBobFacingAce()
    dealt.onMessage(alice, AnswerInsurance(true))
    passTime(dealt, DefaultDecisionTimeout.toSeconds + 1)
    timeouts(bob) shouldBe List(Decision.Insurance)
    timeouts(alice) shouldBe List.empty // she answered in time

  test("nobody is told that the time is up when everybody decides in time"):
    val (dealt, alice, bob) = aliceAndBobAgainstBustingDealer()
    passTime(dealt, DefaultDecisionTimeout.toSeconds - 1) // each one decides just before the time is up
    dealt.onMessage(alice, Stand)
    passTime(dealt, DefaultDecisionTimeout.toSeconds - 1)
    dealt.onMessage(bob, Stand)
    List(alice, bob).foreach(timeouts(_) shouldBe List.empty)

  test("the notice of a late action arrives before the new state of the table"):
    val (dealt, alice, _) = aliceAndBobAgainstBustingDealer()
    passTime(dealt, DefaultDecisionTimeout.toSeconds + 1)
    val afterNotice = alice.inbox.toList.dropWhile(_ != DecisionTimedOut(Decision.Action)).tail
    afterNotice.head shouldBe a[TableUpdate] // the player sees "time is up" and then the turn that goes on
    lastTable(alice).players.head.state shouldBe PlayerState.Standing

  test("a player who does not bet in time is told so and the hand starts without them"):
    val (dealt, alice, bob) = aliceBetsBobIsSilent(aliceHandOnly *)
    passTime(dealt, DefaultDecisionTimeout.toSeconds + 1)
    timeouts(bob) shouldBe List(Decision.Bet)
    timeouts(alice) shouldBe List.empty
    alice.inbox.last shouldBe ActionRequest("Alice", canDoubleDown = true, canSplit = false)
    lastTable(bob).currentTurn shouldBe Some("Alice")

  test("the table does not start the hand before the time to bet is up"):
    val (dealt, alice, _) = aliceBetsBobIsSilent(aliceHandOnly *)
    passTime(dealt, DefaultDecisionTimeout.toSeconds - 1)
    alice.inbox.exists(_.isInstanceOf[ActionRequest]) shouldBe false
    lastTable(alice).dealer.visibleCards shouldBe empty

  test("a player who sits out a hand keeps the seat and the money, and has no cards"):
    val (dealt, alice, bob) = aliceBetsBobIsSilent(aliceHandOnly *)
    passTime(dealt, DefaultDecisionTimeout.toSeconds + 1)
    val bobView = lastTable(alice).players.find(_.name == "Bob").get
    bobView.cards shouldBe empty
    bobView.bet shouldBe None
    bobView.balance shouldBe DefaultBalance
    lastTable(bob).players.map(_.name) shouldBe List("Alice", "Bob")

  test("a bet that arrives after the time is up is refused"):
    val (dealt, _, bob) = aliceBetsBobIsSilent(aliceHandOnly *)
    passTime(dealt, DefaultDecisionTimeout.toSeconds + 1)
    dealt.onMessage(bob, PlaceBet(10))
    bob.inbox.last shouldBe ErrorMessage(NotBettingTime)

  test("the player who sat out is asked to bet again when the next hand starts"):
    val (dealt, alice, bob) = aliceBetsBobIsSilent(aliceHandOnly *)
    passTime(dealt, DefaultDecisionTimeout.toSeconds + 1)
    dealt.onMessage(alice, Stand)
    handResults(bob) shouldBe List(("Alice", DefaultBalance - 10 + 20), ("Bob", DefaultBalance))
    bob.inbox.last shouldBe BetRequest(DefaultBalance, Game.MinGameBet)

  test("the insurance is offered, and waited for, only to the players who bet"):
    val (dealt, alice, bob) = aliceBetsBobIsSilent(Value.Ten, Value.Ace, Value.Six, Value.Six, Value.Seven)
    passTime(dealt, DefaultDecisionTimeout.toSeconds + 1)
    alice.inbox.last shouldBe InsuranceOffer
    bob.inbox.contains(InsuranceOffer) shouldBe false
    dealt.onMessage(alice, AnswerInsurance(false)) // nobody else to wait for
    alice.inbox.last shouldBe ActionRequest("Alice", canDoubleDown = true, canSplit = false)
    timeouts(bob) shouldBe List(Decision.Bet) // and no second notice: Bob was not asked for the insurance

  test("when nobody bets in time no hand is dealt and a new round of bets starts"):
    val dealt = tableWithDeck()
    val alice = join("Alice", dealt)
    val bob = join("Bob", dealt)
    passTime(dealt, DefaultDecisionTimeout.toSeconds + 1)
    List(alice, bob).foreach: client =>
      timeouts(client) shouldBe List(Decision.Bet)
      client.inbox.last shouldBe BetRequest(DefaultBalance, Game.MinGameBet)
      client.inbox.exists(_.isInstanceOf[ActionRequest]) shouldBe false
    lastTable(alice).dealer.visibleCards shouldBe empty

  test("after an empty round of bets the players still get the time to bet"):
    val dealt = tableWithDeck(aliceHandOnly.map(card) *)
    val alice = join("Alice", dealt)
    passTime(dealt, DefaultDecisionTimeout.toSeconds + 1) // empty round: a new one begins at this instant
    passTime(dealt, DefaultDecisionTimeout.toSeconds - 1) // not over yet
    timeouts(alice) shouldBe List(Decision.Bet) // the notice of the first round only
    dealt.onMessage(alice, PlaceBet(10))
    alice.inbox.last shouldBe ActionRequest("Alice", canDoubleDown = true, canSplit = false)

  test("a late bettor who disconnects is not waited for and the hand starts at once"):
    val (dealt, alice, bob) = aliceBetsBobIsSilent(aliceHandOnly *)
    dealt.onDisconnected(bob)
    alice.inbox.last shouldBe ActionRequest("Alice", canDoubleDown = true, canSplit = false)
    timeouts(bob) shouldBe List.empty

  // ---- the balances the players bring to the table and take away ----

  test("a player sits down with the balance kept from the last time"):
    cashier.balances = Map("Alice" -> 137.5)
    val alice = join("Alice")
    alice.inbox.head shouldBe JoinAccepted("Alice", 137.5)
    alice.inbox.last shouldBe BetRequest(137.5, Game.MinGameBet)

  test("a player who cannot sit down here is told why and takes no seat"):
    cashier.refusals = Map("Alice" -> "You are already sitting at another table")
    join("Alice").inbox.toList shouldBe List(JoinRejected("You are already sitting at another table"))
    table.freeSeats shouldBe Game.MaxPlayersNum

  test("a player without the money for the minimum bet is refused, and the money is given back as it was"):
    cashier.balances = Map("Alice" -> 2.5)
    join("Alice").inbox.toList shouldBe List(JoinRejected(NotEnoughMoney))
    cashier.returned.toList shouldBe List("Alice" -> 2.5)

  test("a player who leaves takes away the balance, without the bet lost on the table"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Six, Value.Seven)
    dealt.onMessage(alice, LeaveTable)
    cashier.returned.toList shouldBe List("Alice" -> (DefaultBalance - 10))

  test("a player who leaves after a split takes away the money of every hand"):
    val (dealt, alice) = aliceDealt(splitDeck*)
    dealt.onMessage(alice, Split)
    dealt.onMessage(alice, Stand) // the money left goes on to the split hand
    dealt.onMessage(alice, LeaveTable)
    cashier.returned.toList shouldBe List("Alice" -> (DefaultBalance - 20))

  test("a player who disconnects during a hand takes away the balance once the bets are settled"):
    val (dealt, alice, bob) = aliceAndBobAgainstBustingDealer()
    dealt.onDisconnected(alice)
    cashier.returned.isEmpty shouldBe true // the hand is not over yet
    dealt.onMessage(bob, Stand)
    cashier.returned.toList shouldBe List("Alice" -> (DefaultBalance - 10 + 20))

  test("a player who loses everything leaves with what is left"):
    val (dealt, alice) = aliceBetting(100, Value.Ten, Value.Nine, Value.Six, Value.Seven, Value.Two)
    dealt.onMessage(alice, Stand)
    cashier.returned.toList shouldBe List("Alice" -> 0.0)

  test("when the cut card comes out every player leaves with their balance"):
    val dealt = TableHandler(cashier = cashier, newGame = () => Game(List.empty,
      Deck.testDeck(card(Value.Ten), card(Value.Nine), card(Value.Six), card(Value.Seven), CutCard, card(Value.Two))))
    val alice = join("Alice", dealt)
    dealt.onMessage(alice, PlaceBet(10))
    dealt.onMessage(alice, Stand)
    cashier.returned.toList shouldBe List("Alice" -> (DefaultBalance - 10))

  test("when the table stops in the middle of a hand the bets go back to the players, who leave with their money"):
    val (dealt, alice, bob) = aliceAndBobAgainstBustingDealer()
    dealt.onMessage(alice, Hit) // Alice busts, but the hand does not count any more: her bet goes back too
    dealt.onStop()
    cashier.returned.toSet shouldBe Set("Alice" -> DefaultBalance, "Bob" -> DefaultBalance)
    dealt.freeSeats shouldBe Game.MaxPlayersNum

  test("when the table stops between two hands every player leaves with their balance"):
    val (dealt, alice) = aliceDealt(Value.Ten, Value.Nine, Value.Queen, Value.Seven, Value.Two)
    dealt.onMessage(alice, Stand) // 20 against 18: Alice wins, and is asked for the next bet
    dealt.onStop()
    cashier.returned.toList shouldBe List("Alice" -> (DefaultBalance + 10))


  // ---- the time left is sent with every request, and the table plays the hand of who does not answer ----

  test("every request comes with the time left to answer it"):
    val dealt = tableWithDeck(card(Value.Ten), card(Value.Nine), card(Value.Six), card(Value.Seven))
    val alice = join("Alice", dealt)
    alice.inbox.takeRight(2).toList shouldBe List(TimeToDecide(DefaultDecisionTimeout.toSeconds.toInt), BetRequest(DefaultBalance, Game.MinGameBet))
    dealt.onMessage(alice, PlaceBet(10))
    alice.inbox.takeRight(2).toList shouldBe
      List(TimeToDecide(DefaultDecisionTimeout.toSeconds.toInt), ActionRequest("Alice", canDoubleDown = true, canSplit = false))

  test("when a player sits down while the others are betting, the time to bet starts again for everybody"):
    val dealt = tableWithDeck(card(Value.Ten), card(Value.Nine), card(Value.Six), card(Value.Seven))
    val alice = join("Alice", dealt)
    passTime(dealt, 20)
    val bob = join("Bob", dealt)
    val fullTime = TimeToDecide(DefaultDecisionTimeout.toSeconds.toInt)
    bob.inbox.takeRight(2).head shouldBe fullTime
    alice.inbox.takeRight(2).toList shouldBe List(fullTime, BetRequest(DefaultBalance, Game.MinGameBet))
    passTime(dealt, 20) // 40 seconds after Alice sat down, but only 20 after Bob: nobody is late yet
    alice.inbox.exists(_.isInstanceOf[DecisionTimedOut]) shouldBe false

  test("a player who has already bet is not asked again when somebody sits down"):
    val dealt = tableWithDeck(card(Value.Ten), card(Value.Nine), card(Value.Six), card(Value.Seven))
    val alice = join("Alice", dealt)
    join("Bob", dealt)
    dealt.onMessage(alice, PlaceBet(10))
    val asked = alice.inbox.count(_.isInstanceOf[BetRequest])
    join("Carol", dealt)
    alice.inbox.count(_.isInstanceOf[BetRequest]) shouldBe asked

  /** The values of the cards of a player, as the table shows them. */
  private def cardsOf(name: String, seenBy: FakeConnection): List[Value] =
    lastTable(seenBy).players.find(_.name == name).get.cards.map(_.value)

  test("when time is up a hand worth less than 12 gets cards until it is worth 12 or more, then stands"):
    // deal order: Alice, Bob, dealer, Alice, Bob, dealer face down. Alice 5+4 = 9, then 2 (11) and 5 (16)
    val (dealt, alice, bob) = aliceAndBobWith(Value.Five, Value.Ten, Value.Nine, Value.Four, Value.Eight, Value.Seven,
      Value.Two, Value.Five)
    passTime(dealt, DefaultDecisionTimeout.toSeconds + 1)
    cardsOf("Alice", bob) shouldBe List(Value.Five, Value.Four, Value.Two, Value.Five)
    lastTable(bob).players.map(_.state) shouldBe List(PlayerState.Standing, PlayerState.Active)
    bob.inbox.last shouldBe ActionRequest("Bob", canDoubleDown = true, canSplit = false)

  test("when time is up a hand worth 12 or more stands without new cards"):
    val (dealt, _, bob) = aliceAndBobWith(Value.Ten, Value.Ten, Value.Nine, Value.Two, Value.Eight, Value.Seven, Value.Five)
    passTime(dealt, DefaultDecisionTimeout.toSeconds + 1)
    cardsOf("Alice", bob) shouldBe List(Value.Ten, Value.Two)
    lastTable(bob).players.head.state shouldBe PlayerState.Standing

  test("the best value of the hand counts: an ace and a five are worth 16, and the hand stands"):
    val (dealt, _, bob) = aliceAndBobWith(Value.Ace, Value.Ten, Value.Nine, Value.Five, Value.Eight, Value.Seven, Value.Five)
    passTime(dealt, DefaultDecisionTimeout.toSeconds + 1)
    cardsOf("Alice", bob) shouldBe List(Value.Ace, Value.Five)

  test("the hand of a player who disconnects on their turn is played by the same rule"):
    val (dealt, alice, bob) = aliceAndBobWith(Value.Five, Value.Ten, Value.Nine, Value.Four, Value.Eight, Value.Seven,
      Value.Three)
    dealt.onDisconnected(alice)
    cardsOf("Alice", bob) shouldBe List(Value.Five, Value.Four, Value.Three) // 9, then 12: it stands
    bob.inbox.last shouldBe ActionRequest("Bob", canDoubleDown = true, canSplit = false)
