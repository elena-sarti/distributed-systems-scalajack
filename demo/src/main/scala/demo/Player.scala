package demo

import protocol.{ClientMessage, MessageCodec, ServerMessage}
import protocol.ClientMessage.{AnswerInsurance, JoinTable, PlaceBet, Stand}
import protocol.LobbyApi
import protocol.LobbyApi.{AmountRequest, AssignRequest, BalanceRequest, Funds, LocateRequest, LoggedIn, LoginRequest, RegisterRequest, TableAddress}
import protocol.ServerMessage.{ActionRequest, BetRequest, ErrorMessage, HandResults, InsuranceOffer, JoinAccepted, JoinRejected, TableUpdate}
import protocol.PlayerView
import upickle.default.ReadWriter
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse, WebSocket}
import java.time.{Duration, Instant}
import java.util.concurrent.{CompletionStage, LinkedBlockingQueue, TimeUnit}
import scala.util.Try

/** The lobby of the deployment, reached over its HTTP API like the real client does. */
final class Lobby(baseUrl: String):
  private val http = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(3)).build()

  def register(username: String, password: String): Int = post(LobbyApi.RegisterPath, RegisterRequest(username, password))._1

  def login(username: String, password: String): Option[LoggedIn] =
    post(LobbyApi.LoginPath, LoginRequest(username, password)) match
      case (200, body) => LobbyApi.decode[LoggedIn](body).toOption
      case _           => None

  /** Deposits amount fiches: the new balance, or None if the lobby refused or did not answer. */
  def deposit(token: String, amount: Double): Option[Double] = move(LobbyApi.DepositPath, token, amount).toOption

  /** Withdraws amount fiches: the new balance, or the status of the refusal (0 if the lobby did not answer). */
  def withdraw(token: String, amount: Double): Either[Int, Double] = move(LobbyApi.WithdrawPath, token, amount)

  /** The balance kept by the lobby, or the status of the refusal (0 if the lobby did not answer). */
  def balance(token: String): Either[Int, Double] =
    post(LobbyApi.BalancePath, BalanceRequest(token)) match
      case (200, body) => LobbyApi.decode[Funds](body).map(_.balance).left.map(_ => 200)
      case (status, _) => Left(status)

  private def move(path: String, token: String, amount: Double): Either[Int, Double] =
    post(path, AmountRequest(token, amount)) match
      case (200, body) => LobbyApi.decode[Funds](body).map(_.balance).left.map(_ => 200)
      case (status, _) => Left(status)

  /** Where the lobby sends the player now, or the status of the refusal (0 if the lobby did not answer). */
  def assign(token: String): Either[Int, TableAddress] =
    post(LobbyApi.AssignPath, AssignRequest(token)) match
      case (200, body) => LobbyApi.decode[TableAddress](body).left.map(_ => 200)
      case (status, _) => Left(status)

  /** Where the table of the player is now, or the status of the refusal (0 if no answer). */
  def locate(token: String): Either[Int, TableAddress] =
    post(LobbyApi.LocatePath, LocateRequest(token)) match
      case (200, body) => LobbyApi.decode[TableAddress](body).left.map(_ => 200)
      case (status, _) => Left(status)

  private def post[T: ReadWriter](path: String, body: T): (Int, String) =
    Try:
      val request = HttpRequest.newBuilder(URI.create(baseUrl + path)).timeout(Duration.ofSeconds(5))
        .header("Content-Type", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(LobbyApi.encode(body))).build()
      val response = http.send(request, HttpResponse.BodyHandlers.ofString())
      (response.statusCode(), response.body())
    .getOrElse((0, ""))

/** The connection of a player to a table: what the table sends is queued, in order, until the demo looks at it. */
final class TableConnection private (socket: WebSocket, inbox: LinkedBlockingQueue[Option[ServerMessage]]):
  @volatile private var open = true
  @volatile private var table = Option.empty[protocol.TableView]

  /** The last view of the table among the messages looked at so far. */
  def lastTable: Option[protocol.TableView] = table

  def send(message: ClientMessage): Unit =
    Try(socket.sendText(MessageCodec.encode(message), true).get(5, TimeUnit.SECONDS))

  /** The first message that `wanted` accepts within the timeout, skipping the others. */
  def await[T](timeout: Duration)(wanted: PartialFunction[ServerMessage, T]): Option[T] =
    val end = Instant.now().plus(timeout)
    var result = Option.empty[T]
    while result.isEmpty && open && Instant.now().isBefore(end) do
      Option(inbox.poll(Duration.between(Instant.now(), end).toMillis.max(1), TimeUnit.MILLISECONDS)) match
        case Some(Some(message)) =>
          message match
            case TableUpdate(view) => table = Some(view)
            case _                 => ()
          result = wanted.lift(message)
        case Some(None)          => open = false
        case None                => ()
    result

  /** Whether the table closes the connection (or it drops) within the timeout. */
  def isOpen: Boolean = open

  def awaitClosed(timeout: Duration): Boolean =
    await(timeout)(PartialFunction.empty)
    !open

  def close(): Unit = Try(socket.sendClose(WebSocket.NORMAL_CLOSURE, "").get(2, TimeUnit.SECONDS))

object TableConnection:
  def open(address: TableAddress, token: String): Option[TableConnection] =
    val inbox = LinkedBlockingQueue[Option[ServerMessage]]()
    val listener = new WebSocket.Listener:
      private val text = StringBuilder()
      override def onText(socket: WebSocket, data: CharSequence, last: Boolean): CompletionStage[?] =
        text.append(data)
        if last then
          MessageCodec.decodeServerMessage(text.toString).foreach(message => inbox.put(Some(message)))
          text.clear()
        socket.request(1)
        null
      override def onClose(socket: WebSocket, code: Int, reason: String): CompletionStage[?] =
        inbox.put(None)
        null
      override def onError(socket: WebSocket, error: Throwable): Unit = inbox.put(None)
    Try:
      HttpClient.newHttpClient().newWebSocketBuilder()
        .header("Authorization", s"Bearer $token")
        .connectTimeout(Duration.ofSeconds(3))
        .buildAsync(URI.create(s"ws://${address.host}:${address.wsPort}"), listener)
        .get(5, TimeUnit.SECONDS)
    .toOption.map(TableConnection(_, inbox))

/** A player sitting at a table: where, through which connection, and with how much money. */
final case class Seat(address: TableAddress, connection: TableConnection, balance: Double)

/** A player of the demo: a new user, registered and logged in at the lobby, who has made a first deposit. */
final class Player private (lobby: Lobby, val name: String, token: String, val initialBalance: Double):

  /** Asks the lobby for a table, as many times as needed within the timeout, and sits down there. */
  def sitDown(timeout: Duration = Duration.ofSeconds(30)): Either[String, Seat] = untilSeated(timeout)(lobby.assign(token))

  /** After losing the table: asks the lobby where it is now (`/locate`), as many times as needed within the timeout,
   * and sits down there again with the same name, which gives the seat back. This is what the client does. */
  def comeBack(timeout: Duration = Duration.ofSeconds(30)): Either[String, Seat] = untilSeated(timeout)(lobby.locate(token))

  private def untilSeated(timeout: Duration)(ask: => Either[Int, TableAddress]): Either[String, Seat] =
    val end = Instant.now().plus(timeout)
    var result: Either[String, Seat] = Left("no table was given")
    var done = false
    while !done && Instant.now().isBefore(end) do
      ask match
        case Right(address) =>
          result = join(address)
          done = result.isRight
        case Left(status) => result = Left(s"the lobby answered $status")
      if !done then Thread.sleep(1000)
    result

  private def join(address: TableAddress): Either[String, Seat] =
    TableConnection.open(address, token) match
      case None => Left(s"cannot connect to ${address.host}:${address.wsPort}")
      case Some(connection) =>
        connection.send(JoinTable(name))
        connection.await(Duration.ofSeconds(5)) {
          case JoinAccepted(_, balance) => Right(Seat(address, connection, balance))
          case JoinRejected(reason)     => Left(reason)
        }.getOrElse(Left("no answer to the request to sit down"))

  /** Bets and waits until the cards are dealt to the player. */
  def betAndAwaitCards(seat: Seat, amount: Int, timeout: Duration = Duration.ofSeconds(10)): Boolean =
    seat.connection.send(PlaceBet(amount))
    awaitCards(seat, timeout)

  def awaitCards(seat: Seat, timeout: Duration): Boolean =
    seat.connection.await(timeout) {
      case TableUpdate(table) if table.players.exists(p => p.name == name && p.cards.nonEmpty) => true
    }.isDefined

  /** What the table shows of this player once nothing more arrives for `quiet`: the state the player is left in. */
  def settledView(seat: Seat, quiet: Duration = Duration.ofSeconds(1)): Option[PlayerView] =
    while seat.connection.await(quiet) { case message => message }.isDefined do ()
    seat.connection.lastTable.flatMap(_.players.find(_.name == name))

  /** The first view of the table that shows this player: right after sitting down again, the state of the seat. */
  def awaitView(seat: Seat, timeout: Duration = Duration.ofSeconds(5)): Option[PlayerView] =
    seat.connection.await(timeout) { case TableUpdate(table) if table.players.exists(_.name == name) =>
      table.players.find(_.name == name).get }

  /** Plays the hand in progress to its end, standing at once and refusing the insurance: the balance afterward, or
   * the current one if no hand is being played. */
  def finishHand(seat: Seat, timeout: Duration = Duration.ofSeconds(30)): Option[Double] =
    val end = Instant.now().plus(timeout)
    var result = Option.empty[Double]
    while result.isEmpty && seat.connection.isOpen && Instant.now().isBefore(end) do
      seat.connection.await(Duration.between(Instant.now(), end)) { case message => message } match
        case Some(InsuranceOffer)         => seat.connection.send(AnswerInsurance(false))
        case Some(ActionRequest(_, _, _)) => seat.connection.send(Stand)
        case Some(HandResults(balances))  => result = balances.collectFirst { case (`name`, balance) => balance }
        case Some(BetRequest(balance, _)) => result = Some(balance) // no hand was in progress
        case _                            => ()
    result

  /** The user tries to bet: if the bet is taken the cards are dealt, otherwise it shows the reason the table gives. */
  def tryToBet(seat: Seat, amount: Int, timeout: Duration = Duration.ofSeconds(5)): Either[String, Unit] =
    seat.connection.send(PlaceBet(amount))
    seat.connection.await(timeout) {
      case ErrorMessage(reason) => Left(reason)
      case TableUpdate(table) if table.players.exists(p => p.name == name && p.cards.nonEmpty) => Right(())
    }.getOrElse(Left("no answer"))

object Player:
  val Password = "demo-password"

  /** A new account has no money and the lobby refuses a seat below the minimum bet: every player deposits this first. */
  val FirstDeposit = 100.0

  /** A new user, whose name tells the scenario and is different at every run. */
  def signUp(lobby: Lobby, prefix: String): Option[Player] =
    val name = s"$prefix${System.currentTimeMillis() % 1_000_000}"
    lobby.register(name, Password)
    for
      session <- lobby.login(name, Password)
      balance <- lobby.deposit(session.token, FirstDeposit) // without money the lobby gives no seat
    yield Player(lobby, name, session.token, balance)
