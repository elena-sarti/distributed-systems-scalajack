package demo

import protocol.{PlayerView, TableView}
import protocol.ClientMessage.{AnswerInsurance, JoinTable, LeaveTable, PlaceBet, Stand}
import protocol.ServerMessage.{ActionRequest, ErrorMessage, HandResults, InsuranceOffer, JoinAccepted, JoinRejected, TableUpdate}
import utils.ModelExports.{StandardCard, calculateScore}
import java.time.{Duration, Instant}
import scala.util.control.NonFatal

/** The end-to-end tests: the whole system of docker-compose.yml started from zero (an empty database) and used over
 * the network as the client uses it, from the registration to leaving the table.
 *
 * `sbt "demo/runMain demo.EndToEnd"` starts the deployment afresh: it deletes the database of the local deployment.
 * With `--running` it tests the deployment that is already running instead, at `--lobby <url>` if it is not the one of
 * docker-compose.yml. */
object EndToEnd:
  private val Bet       = 10
  private val Deposit   = 100.0
  private val Password  = "e2e-password"
  private val HandTime  = Duration.ofSeconds(45) // a hand where every player answers at once ends well before this
  private val SeatFreed = Duration.ofSeconds(15) // the table hands the balance back to the lobby within this

  def main(args: Array[String]): Unit =
    val url   = args.sliding(2).collectFirst { case Array("--lobby", value) => value }.getOrElse("http://localhost:9080")
    val lobby = Lobby(url)
    if !args.contains("--running") then startFromZero()
    val tests = List(
      "Registration, login and the money in the lobby" -> accounts,
      "A whole hand: sit down, bet, play, leave"        -> oneHand,
      "Two players at the same table"                   -> twoPlayers,
      "The table accepts only the players the lobby has logged in" -> tokens)
    val reports = tests.map: (title, test) =>
      println(s"\n=== $title ===")
      val report = Report(title)
      try test(lobby, report)
      catch case NonFatal(error) => report.check(s"the test ran to the end (${error.getMessage})", met = false)
      report
    println("\n=== Outcome ===")
    reports.foreach(report => println(s"  ${if report.passed then "[OK]  " else "[FAIL]"} ${report.scenario}"))
    sys.exit(if reports.forall(_.passed) then 0 else 1)

  /** An empty database and every node started again: `down -v` deletes the volume of the database too. */
  private def startFromZero(): Unit =
    println("Starting the deployment from zero (docker compose down -v, then up)...")
    Docker.must("compose", "down", "-v")
    Docker.must("compose", "up", "-d", "--wait")
    val ready = Docker.awaitLog("table-1", Duration.ofSeconds(120))(_.contains("registered at the lobby"))
    if ready.isEmpty then
      println("table-1 did not register at the lobby within 2 minutes: the tests cannot start")
      sys.exit(1)

  /** A name never used before, so that the tests can run again on the same database. */
  private def newName(prefix: String): String = s"$prefix${System.currentTimeMillis() % 1_000_000}"

  /** Registers a new user and logs in: the name and the session token. */
  private def newUser(lobby: Lobby, prefix: String): (String, String) =
    val name = newName(prefix)
    lobby.register(name, Password)
    val session = lobby.login(name, Password).getOrElse(throw IllegalStateException(s"$name cannot log in"))
    (name, session.token)

  private def accounts(lobby: Lobby, report: Report): Unit =
    val name = newName("e2e")
    report.check("a new user is registered", lobby.register(name, Password) == 201)
    report.check("the same name cannot be registered twice, in any case", lobby.register(name.toUpperCase, Password) == 409)
    report.check("a password that is too short is refused", lobby.register(newName("short"), "short") == 400)
    report.check("a wrong password does not log in", lobby.login(name, "wrong-password").isEmpty)
    val session = lobby.login(name, Password)
    report.check("the right password logs in, and a new account has no fiches", session.exists(_.balance == 0))
    session.foreach: s =>
      report.check("without fiches the lobby gives no seat (403)", lobby.assign(s.token) == Left(403))
      report.check("an amount that is not made of fiches (22.34) cannot be deposited", lobby.deposit(s.token, 22.34).isEmpty)
      report.check(s"a deposit of $Deposit is added to the balance", lobby.deposit(s.token, Deposit).contains(Deposit))
      report.check("more than the balance cannot be withdrawn (409)", lobby.withdraw(s.token, Deposit + 50) == Left(409))
      report.check("a withdrawal is taken from the balance", lobby.withdraw(s.token, 20) == Right(Deposit - 20))
      report.check("the balance kept by the lobby is the same", lobby.balance(s.token) == Right(Deposit - 20))
      report.check("a forged token is refused (401)", lobby.balance(s.token.reverse) == Left(401))

  private def oneHand(lobby: Lobby, report: Report): Unit =
    val (name, token) = newUser(lobby, "hand")
    lobby.deposit(token, Deposit)
    seat(lobby, name, token, report).foreach: connection =>
      report.check("money cannot be moved while the player is seated (409)", lobby.withdraw(token, 10) == Left(409))
      connection.send(PlaceBet(Bet))
      val dealt = connection.await(Duration.ofSeconds(10)) { case TableUpdate(t) if cardsOf(t, name).size >= 2 => t }
      report.check(s"the bet of $Bet is taken and two cards are dealt", dealt.isDefined)
      dealt.foreach(t => report.note(s"cards: ${describe(cardsOf(t, name))}, dealer shows ${describe(t.dealer.visibleCards)}"))
      val results = playToTheEnd(connection)
      val lastView  = connection.lastTable
      val balance = results.flatMap(_.collectFirst { case (`name`, b) => b })
      report.check(s"the hand ends with the results (balance ${balance.getOrElse("-")})", balance.isDefined)
      for t <- lastView; b <- balance do
        report.note(s"dealer: ${describe(t.dealer.visibleCards)}, player: ${describe(cardsOf(t, name))}")
        report.check("the dealer stands at 17 or more, or busts", dealerFollowedTheRules(t.dealer.visibleCards))
        report.check(s"the balance is the one the rules give (${expectedBalance(Deposit, t, name)})",
          expectedBalance(Deposit, t, name) == b)
      connection.send(LeaveTable)
      connection.close()
      val back = waitFor(SeatFreed)(lobby.locate(token) == Left(404) && lobby.balance(token) == Right(balance.getOrElse(-1.0)))
      report.check("after leaving, the lobby is told the seat is free and keeps the balance of the table", back)
      report.check("all the fiches can be withdrawn", balance.exists(b => lobby.withdraw(token, b) == Right(0.0)))

  private def twoPlayers(lobby: Lobby, report: Report): Unit =
    val users = List(newUser(lobby, "ann"), newUser(lobby, "ben"))
    users.foreach((_, token) => lobby.deposit(token, Deposit))
    val seated = users.flatMap((name, token) => seat(lobby, name, token, report).map(c => (name, token, c)))
    if seated.size == 2 then
      val tables = seated.flatMap((_, token, _) => lobby.locate(token).toOption.map(_.tableId)).distinct
      report.check(s"the lobby sends both to the same table (${tables.mkString(", ")})", tables.size == 1)
      seated.foreach((_, _, connection) => connection.send(PlaceBet(Bet)))
      val bothDealt = seated.forall: (_, _, connection) =>
        connection.await(Duration.ofSeconds(10)) {
          case TableUpdate(t) if seated.forall((n, _, _) => cardsOf(t, n).size >= 2) => t }.isDefined
      report.check("the cards are dealt to both, and each sees the cards of the other", bothDealt)
      val results = seated.map((_, _, connection) => playToTheEnd(connection))
      report.check("both get the results of the same hand", results.forall(_.isDefined) && results.distinct.size == 1)
      val dealers = seated.flatMap((_, _, connection) => connection.lastTable.map(_.dealer))
      report.check("both see the same cards of the dealer", dealers.size == 2 && dealers.distinct.size == 1)
      seated.foreach: (name, _, connection) =>
        for t <- connection.lastTable; r <- results.head; b <- r.collectFirst { case (`name`, b) => b } do
          report.check(s"the balance of $name is the one the rules give", expectedBalance(Deposit, t, name) == b)
      seated.foreach((_, _, connection) => { connection.send(LeaveTable); connection.close() })
      val freed = waitFor(SeatFreed)(seated.forall((_, token, _) => lobby.locate(token) == Left(404)))
      report.check("both seats are freed when they leave", freed)

  private def tokens(lobby: Lobby, report: Report): Unit =
    val (_, token) = newUser(lobby, "tok")
    lobby.deposit(token, Deposit)
    lobby.assign(token).foreach: address =>
      val forged = TableConnection.open(address, token.reverse)
      report.check("a connection with a forged token is refused", forged.forall(c => c.awaitClosed(Duration.ofSeconds(3))))
      TableConnection.open(address, token).foreach: connection =>
        connection.send(JoinTable("someone-else"))
        val answer = connection.await(Duration.ofSeconds(5)) { case JoinRejected(reason) => reason; case JoinAccepted(_, _) => "accepted" }
        report.check(s"a player cannot sit down under the name of another user (${answer.getOrElse("no answer")})",
          answer.exists(_ != "accepted"))
        connection.close()

  /** Asks the lobby for a table and sits down there. */
  private def seat(lobby: Lobby, name: String, token: String, report: Report): Option[TableConnection] =
    val connection = lobby.assign(token).toOption.flatMap(TableConnection.open(_, token))
    val joined = connection.flatMap: c =>
      c.send(JoinTable(name))
      c.await(Duration.ofSeconds(5)) { case JoinAccepted(_, balance) => balance }
    report.check(s"$name gets a seat from the lobby and sits down with ${joined.getOrElse("-")} fiches", joined.contains(Deposit))
    connection.filter(_ => joined.isDefined)

  /** Plays the hand as a careful player: no insurance, and stands at once. The balances at the end of the hand. */
  private def playToTheEnd(connection: TableConnection): Option[List[(String, Double)]] =
    val end = Instant.now().plus(HandTime)
    var results = Option.empty[List[(String, Double)]]
    while results.isEmpty && connection.isOpen && Instant.now().isBefore(end) do
      connection.await(Duration.between(Instant.now(), end)) { case message => message } match
        case Some(InsuranceOffer)          => connection.send(AnswerInsurance(false))
        case Some(ActionRequest(_, _, _))  => connection.send(Stand)
        case Some(HandResults(balances))   => results = Some(balances)
        case Some(ErrorMessage(reason))    => println(s"  the table says: $reason")
        case _                             => ()
    results

  private def cardsOf(table: TableView, name: String): List[StandardCard] =
    table.players.find(_.name == name).map(_.cards).getOrElse(Nil)

  private def value(cards: List[StandardCard]): Int = cards.calculateScore.playableValue
  private def blackjack(cards: List[StandardCard]): Boolean = cards.size == 2 && value(cards) == 21

  private def dealerFollowedTheRules(dealer: List[StandardCard]): Boolean =
    value(dealer) >= 17 && (dealer.size <= 2 || value(dealer.init) < 17)

  /** The balance after the hand of a player who bet [[Bet]] and stood at once, by the rules of the game: a blackjack
   * pays 3:2, a win 1:1, a tie gives the bet back. */
  private def expectedBalance(before: Double, table: TableView, name: String): Double =
    val player = cardsOf(table, name)
    val dealer = table.dealer.visibleCards
    val net: Double =
      if blackjack(player) && blackjack(dealer) then 0.0
      else if blackjack(player) then Bet * 1.5
      else if blackjack(dealer) then -Bet
      else if value(dealer) > 21 || value(player) > value(dealer) then Bet
      else if value(player) == value(dealer) then 0.0
      else -Bet
    before + net

  private def describe(cards: List[StandardCard]): String =
    s"${cards.map(_.value).mkString(" ")} (${if cards.isEmpty then 0 else value(cards)})"

  private def waitFor(timeout: Duration)(condition: => Boolean): Boolean =
    val end = Instant.now().plus(timeout)
    var met = condition
    while !met && Instant.now().isBefore(end) do
      Thread.sleep(500)
      met = condition
    met
