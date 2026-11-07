package network

import cats.effect.{IO, Ref}
import protocol.ClientMessage.JoinTable
import protocol.LobbyApi.TableAddress
import protocol.{MessageCodec, ServerMessage}
import protocol.ServerMessage.{JoinAccepted, JoinRejected, TableUpdate}
import utils.TestExports.*
import scala.concurrent.duration.*

class TableReconnectionTest extends AnyFunSuite:

  /** A lobby that gives the answers of the script, one per request (the last one again when the script is over). */
  private final class ScriptedLobby(script: LocateAnswer*) extends LobbyConnection:
    private val answers = Ref.unsafe[IO, List[LocateAnswer]](script.toList)
    val tokens          = Ref.unsafe[IO, List[String]](Nil)
    def locate(token: String): IO[LocateAnswer] =
      tokens.update(_ :+ token) *> answers.modify:
        case only :: Nil  => (List(only), only)
        case next :: rest => (rest, next)
        case Nil          => (Nil, LocateAnswer.Unreachable)

  private def withTable(answer: String => Option[ServerMessage])(test: FakeTableServer => Unit): Unit =
    val server = FakeTableServer()
    server.reply = text => answer(text).map(MessageCodec.encode)
    server.start()
    try test(server)
    finally server.stop()

  /** A table that gives Alice her seat back with 90 left, and then shows her the table. */
  private val giveBackTheSeat: String => Option[ServerMessage] = text =>
    MessageCodec.decodeClientMessage(text).toOption.collect { case JoinTable(name) => JoinAccepted(name, 90.0) }

  private def at(server: FakeTableServer) = LocateAnswer.Found(TableAddress("table-1", "localhost", server.uri.getPort))

  private def reconnect(lobby: LobbyConnection) =
    TableReconnection.reconnect(lobby, "a.b.c", "Alice", retryEvery = 50.millis, giveUpAfter = 2.seconds)

  test("the client asks the lobby where its table is, connects there with its token and gets its seat back"):
    withTable(giveBackTheSeat): server =>
      val lobby    = ScriptedLobby(at(server))
      val balance  = reconnect(lobby).use(reseated => IO.pure(reseated.balance)).timeout(10.seconds).unsafeRunSync()
      balance shouldBe 90.0
      MessageCodec.decodeClientMessage(server.nextReceived()) shouldBe Right(JoinTable("Alice"))
      server.lastAuthorization shouldBe Some("Bearer a.b.c")
      lobby.tokens.get.unsafeRunSync() shouldBe List("a.b.c")

  test("while the backup is taking over the client asks again, until the table is reachable"):
    withTable(giveBackTheSeat): server =>
      val lobby = ScriptedLobby(LocateAnswer.NotReachableYet, LocateAnswer.Unreachable, at(server))
      reconnect(lobby).use(_ => IO.unit).timeout(10.seconds).unsafeRunSync()
      lobby.tokens.get.unsafeRunSync() should have size 3

  test("what the table sends after the seat is given back is left for the client"):
    val view = protocol.TableView(List.empty, protocol.DealerView(List.empty, 0), None, 7)
    withTable(text => giveBackTheSeat(text)): server =>
      val event = reconnect(ScriptedLobby(at(server))).use: reseated =>
        IO(server.sendToClients(MessageCodec.encode(TableUpdate(view)))) *> reseated.connection.receive
      .timeout(10.seconds).unsafeRunSync()
      event shouldBe ConnectionEvent.Received(TableUpdate(view))

  test("a node that is not listening yet is tried again"):
    withTable(giveBackTheSeat): server =>
      val nobody = LocateAnswer.Found(TableAddress("table-1", "localhost", 1))
      val lobby  = ScriptedLobby(nobody, at(server))
      reconnect(lobby).use(_ => IO.unit).timeout(10.seconds).unsafeRunSync()
      lobby.tokens.get.unsafeRunSync() should have size 2

  test("a player who sits nowhere any more has lost the seat: the client is told, and does not try again"):
    val lobby  = ScriptedLobby(LocateAnswer.NotSeated)
    val result = reconnect(lobby).use(_ => IO.unit).attempt.timeout(10.seconds).unsafeRunSync()
    result.left.toOption.get shouldBe a[SeatLostException]
    lobby.tokens.get.unsafeRunSync() should have size 1

  test("a table that refuses the player (the seat was given up) is a lost seat too"):
    withTable(_ => Some(JoinRejected("This name is already used at the table"))): server =>
      val result = reconnect(ScriptedLobby(at(server))).use(_ => IO.unit).attempt.timeout(10.seconds).unsafeRunSync()
      result.left.toOption.get.getMessage shouldBe "This name is already used at the table"

  test("a lobby that refuses the request, e.g. for an expired token, is not asked again"):
    val lobby  = ScriptedLobby(LocateAnswer.Refused(401))
    val result = reconnect(lobby).use(_ => IO.unit).attempt.timeout(10.seconds).unsafeRunSync()
    result.left.toOption.get shouldBe a[ReconnectionFailedException]
    lobby.tokens.get.unsafeRunSync() should have size 1

  test("if the table stays unreachable the client gives up after the time allowed"):
    val result = reconnect(ScriptedLobby(LocateAnswer.NotReachableYet)).use(_ => IO.unit).attempt
      .timeout(10.seconds).unsafeRunSync()
    result.left.toOption.get shouldBe a[ReconnectionFailedException]
