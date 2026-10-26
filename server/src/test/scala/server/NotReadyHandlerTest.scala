package server

import utils.TestExports.*
import protocol.{ClientMessage, ServerMessage}
import scala.collection.mutable.ListBuffer

class NotReadyHandlerTest extends AnyFunSuite:

  test("every message is answered with an error saying the table is not ready"):
    val sent = ListBuffer[ServerMessage]()
    val connection = new Connection:
      val id = "test"
      def send(message: ServerMessage): Unit = sent += message

    NotReadyHandler.onMessage(connection, ClientMessage.Hit)

    sent.toList shouldBe List(ServerMessage.ErrorMessage(NotReadyHandler.Reason))
