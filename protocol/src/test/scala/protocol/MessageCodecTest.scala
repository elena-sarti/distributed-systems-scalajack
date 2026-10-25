package protocol

import utils.TestExports.*

class MessageCodecTest extends AnyFunSuite:

  test("a string that is not JSON is rejected without throwing"):
    MessageCodec.decodeClientMessage("this is not json").isLeft shouldBe true

  test("a message of an unknown kind is rejected"):
    MessageCodec.decodeClientMessage("""{"$type":"Teleport"}""").isLeft shouldBe true

  test("a message with a field of the wrong type is rejected"):
    val validJson = MessageCodec.encode(ClientMessage.PlaceBet(10))
    MessageCodec.decodeClientMessage(validJson.replace("10", "\"ten\"")).isLeft shouldBe true

  test("a client message is not accepted as a server message"):
    MessageCodec.decodeServerMessage(MessageCodec.encode(ClientMessage.Hit)).isLeft shouldBe true

  test("a server message is not accepted as a client message"):
    MessageCodec.decodeClientMessage(MessageCodec.encode(ServerMessage.InsuranceOffer)).isLeft shouldBe true
