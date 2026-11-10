package protocol

import utils.TestExports.*
import protocol.ClientMessage.*

class ClientMessageTest extends AnyFunSuite:

  /** One sample for each kind of client message. */
  private val samples: List[ClientMessage] = List(
    JoinTable("Alice"),
    PlaceBet(10),
    Hit,
    Stand,
    DoubleDown,
    Split,
    AnswerInsurance(true),
    LeaveTable
  )

  samples.foreach: message =>
    test(s"${message.productPrefix} survives a JSON round-trip"):
      MessageCodec.decodeClientMessage(MessageCodec.encode(message)) shouldBe Right(message)

  test("every kind of client message has a round-trip sample"):
    samples.map(_.productPrefix).toSet shouldBe EnumCases.names[ClientMessage]
