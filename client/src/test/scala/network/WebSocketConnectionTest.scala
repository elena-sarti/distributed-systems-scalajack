package network

import cats.effect.IO
import cats.syntax.all.*
import protocol.{ClientMessage, MessageCodec, ServerMessage}
import utils.TestExports.*
import scala.concurrent.duration.*

class WebSocketConnectionTest extends AnyFunSuite:

  private def withServer(test: FakeTableServer => Unit): Unit =
    val server = FakeTableServer()
    server.start()
    try test(server)
    finally server.stop()

  /** Opens a connection, waits until the server has registered it, runs `use`, then closes it. */
  private def withConnection[A](server: FakeTableServer)(use: ServerConnection => IO[A]): A =
    val program = WebSocketConnection.connect(server.uri).use: connection =>
      IO.blocking(server.awaitClientConnected()) *> use(connection)
    program.timeout(10.seconds).unsafeRunSync()

  test("a message sent by the client reaches the server as JSON"):
    withServer: server =>
      withConnection(server)(_.send(ClientMessage.JoinTable("Alice")))
      MessageCodec.decodeClientMessage(server.nextReceived()) shouldBe Right(ClientMessage.JoinTable("Alice"))

  test("a message sent by the server is received by the client"):
    withServer: server =>
      val event = withConnection(server): connection =>
        IO(server.sendToClients(MessageCodec.encode(ServerMessage.JoinAccepted("Alice", 100.0)))) *> connection.receive
      event shouldBe ConnectionEvent.Received(ServerMessage.JoinAccepted("Alice", 100.0))

  test("messages from the server are received in the order they were sent"):
    withServer: server =>
      val sent = (1 to 50).map(i => ServerMessage.BetRequest(i.toDouble, 1))
      val events = withConnection(server): connection =>
        IO(sent.foreach(m => server.sendToClients(MessageCodec.encode(m)))) *>
          List.fill(50)(connection.receive).sequence
      events shouldBe sent.map(ConnectionEvent.Received(_)).toList

  test("messages sent concurrently from many fibers are all delivered"):
    withServer: server =>
      withConnection(server): connection =>
        (1 to 50).toList.parTraverse_(i => connection.send(ClientMessage.PlaceBet(i)))
      val amounts = (1 to 50).flatMap: _ =>
        MessageCodec.decodeClientMessage(server.nextReceived()).toOption.collect:
          case ClientMessage.PlaceBet(amount) => amount
      amounts.toSet shouldBe (1 to 50).toSet

  test("a malformed frame becomes an Invalid event and the connection stays usable"):
    withServer: server =>
      val events = withConnection(server): connection =>
        IO:
          server.sendToClients("this is not json")
          server.sendToClients(MessageCodec.encode(ServerMessage.InsuranceOffer))
        *> List.fill(2)(connection.receive).sequence
      events.head match
        case ConnectionEvent.Invalid(_) => succeed
        case other                      => fail(s"Expected Invalid, got $other")
      events(1) shouldBe ConnectionEvent.Received(ServerMessage.InsuranceOffer)

  test("when the server drops the connection the client sees Disconnected, again and again"):
    withServer: server =>
      val events = withConnection(server): connection =>
        IO(server.disconnectClients()) *> List.fill(3)(connection.receive).sequence
      events.forall(_.isInstanceOf[ConnectionEvent.Disconnected]) shouldBe true

  test("sending after the connection is lost fails with ConnectionClosedException"):
    withServer: server =>
      val result = withConnection(server): connection =>
        IO(server.disconnectClients()) *> connection.receive *> connection.send(ClientMessage.Hit).attempt
      result match
        case Left(_: ConnectionClosedException) => succeed
        case other                              => fail(s"Expected ConnectionClosedException, got $other")

  test("leaving the resource closes the connection on the server"):
    withServer: server =>
      withConnection(server)(_ => IO.unit)
      server.awaitClientDisconnected()
      server.connectionCount shouldBe 0

  test("connecting to a server that is not listening fails"):
    val server = FakeTableServer()
    server.start()
    val uri = server.uri
    server.stop()
    val result = WebSocketConnection.connect(uri).use(_ => IO.unit).timeout(10.seconds).attempt.unsafeRunSync()
    result.isLeft shouldBe true
