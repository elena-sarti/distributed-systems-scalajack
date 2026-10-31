package protocol

import utils.TestExports.*
import utils.ModelExports.{PlayerState, StandardCard, Suit, Value}
import protocol.ServerMessage.*

class ServerMessageTest extends AnyFunSuite:

  private val table = TableView(
    players = List(
      PlayerView("Alice", List(StandardCard(Suit.Hearts, Value.Ace), StandardCard(Suit.Clubs, Value.King)),
        PlayerState.Blackjack, 90.0, Some(10)),
      PlayerView("Bob", List.empty, PlayerState.Active, 100.5, None)
    ),
    dealer = DealerView(List(StandardCard(Suit.Spades, Value.Seven)), hiddenCards = 1),
    currentTurn = Some("Bob"),
    version = 42
  )

  /** One sample for each kind of server message. */
  private val samples: List[ServerMessage] = List(
    JoinAccepted("Alice", 100.0),
    JoinRejected("The table is full"),
    BetRequest(100.0, 5),
    InsuranceOffer,
    ActionRequest("Alice_split1", canDoubleDown = true, canSplit = false),
    TableUpdate(table),
    HandResults(List("Alice" -> 125.0, "Bob" -> 90.0)),
    GameOver(List("Alice" -> 125.0)),
    ErrorMessage("It is not your turn"),
    DecisionTimedOut(Decision.Bet),
    DecisionTimedOut(Decision.Insurance),
    DecisionTimedOut(Decision.Action)
  )

  samples.foreach: message =>
    test(s"$message survives a JSON round-trip"):
      MessageCodec.decodeServerMessage(MessageCodec.encode(message)) shouldBe Right(message)

  test("every kind of server message has a round-trip sample"):
    samples.map(_.productPrefix).toSet shouldBe EnumCases.names[ServerMessage]
