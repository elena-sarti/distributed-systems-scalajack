package demo

import protocol.PlayerView
import java.time.Duration
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

/** What a scenario has observed: each expectation, met or not, with what was seen. */
final class Report(val scenario: String):
  private val lines = ListBuffer.empty[(Boolean, String)]

  def check(expectation: String, met: Boolean): Boolean =
    lines += ((met, expectation))
    println(s"  ${if met then "[OK]  " else "[FAIL]"} $expectation")
    met

  def note(text: String): Unit = println(s"         $text")

  def passed: Boolean = lines.nonEmpty && lines.forall(_._1)

/** The failures of the decision record played on the real deployment of `docker-compose.yml`: the demo starts it
 * afresh, sends a player to the table, breaks a node or the network with Docker, and prints what it observes.
 *
 * Run from the root of the repository, after building the image once (`docker compose build`):
 * {{{
 *   sbt "demo/run"            all the scenarios
 *   sbt "demo/run crash"      only one of them: crash, partition, backup
 * }}}
 */
object FailureDemo:
  private val lobby = Lobby("http://localhost:9080")

  private val Primary = "table-1"
  private val Backup  = "table-1-backup"
  private val LobbyService = "lobby"

  private val PrimaryPort  = 9091
  private val PromotedPort = 9191
  private val Bet          = 10

  /** Where a failure must have been noticed and acted upon: heartbeat timeout, plus a period for the lobby to notice
   * it too, plus a period for the request to be repeated, with some margin for Docker. */
  private val Reaction = Duration.ofSeconds(20)

  private val scenarios: List[(String, String, () => Report)] = List(
    ("crash", "The primary crashes", crash),
    ("partition", "The primary is cut off from everybody, then the network comes back", partition),
    ("backup", "The backup is cut off from everybody, then the network comes back", backupCutOff))

  def main(args: Array[String]): Unit =
    val chosen = if args.isEmpty then scenarios else scenarios.filter((name, _, _) => args.contains(name))
    if chosen.isEmpty then
      println(s"Unknown scenario: ${args.mkString(" ")}. The scenarios are: ${scenarios.map(_._1).mkString(", ")}")
      sys.exit(2)
    val reports = chosen.map: (_, title, play) =>
      println(s"\n=== $title ===")
      try play()
      catch case NonFatal(error) =>
        val report = Report(title)
        report.check(s"the scenario ran to the end (${error.getMessage})", met = false)
        report
    println("\n=== Outcome ===")
    reports.foreach(report => println(s"  ${if report.passed then "[OK]  " else "[FAIL]"} ${report.scenario}"))
    println("\nThe deployment is left as the last scenario ended it: `docker compose down` stops it.")
    sys.exit(if reports.forall(_.passed) then 0 else 1)

  /** A fresh deployment, with table-1 ready to take players. */
  private def freshStart(report: Report): Unit =
    println("  starting the deployment afresh...")
    Docker.freshDeployment()
    val ready = Docker.awaitLog(Primary, Duration.ofSeconds(90))(_.contains("registered at the lobby"))
    report.check("the primary is running and registered at the lobby", ready.isDefined)

  /** A new player sits at the table the lobby assigns, which must be the primary, and gets the cards of a hand. */
  private def playerInAHand(report: Report, prefix: String): Option[(Player, Seat)] =
    seatedPlayer(report, prefix).filter: (player, seat) =>
      report.check(s"${player.name} bets $Bet and gets the cards", player.betAndAwaitCards(seat, Bet))

  /** A new player, registered and logged in, sitting at the table the lobby assigns, which must be the primary. */
  private def seatedPlayer(report: Report, prefix: String): Option[(Player, Seat)] =
    Player.signUp(lobby, prefix) match
      case None =>
        report.check("a new player registers and logs in at the lobby", met = false)
        None
      case Some(player) =>
        player.sitDown() match
          case Left(reason) =>
            report.check(s"${player.name} sits at the table ($reason)", met = false)
            None
          case Right(seat) =>
            Option.when(report.check(s"${player.name} sits at the primary (port ${seat.address.wsPort})",
              seat.address.wsPort == PrimaryPort))((player, seat))

  private def seconds(duration: Duration): String = f"${duration.toMillis / 1000.0}%.1f s"

  private def promotionGranted(line: String): Boolean = line.contains("promotion to primary") && line.endsWith("Granted")

  private def describe(view: Option[PlayerView]): String =
    view.fold("nothing")(v => s"cards ${v.cards.map(card => s"${card.value}-${card.suit}").mkString(" ")}, balance ${v.balance}")

  /** The player has lost the table: like the client, it asks the lobby where the table is now and sits down again. It
   * must find its seat as it left it (`before`), and finish the hand there: lost, pushed or won, never anything else. */
  private def backOnThePromotedNode(report: Report, player: Player, before: Option[PlayerView]): Unit =
    val announced = Docker.awaitLog(LobbyService, Reaction)(_.contains(s"registered at localhost:$PromotedPort"))
    report.check("the promoted node announces itself to the lobby as the table", announced.isDefined)
    player.comeBack() match
      case Left(reason) => report.check(s"${player.name} gets the seat back ($reason)", met = false)
      case Right(seat) =>
        report.check(s"asked where the table is now (/locate), the lobby sends ${player.name} to the promoted node (port ${seat.address.wsPort})",
          seat.address.wsPort == PromotedPort)
        val after = player.awaitView(seat)
        report.check(s"${player.name} is back in the same seat, as it was: ${describe(after)}",
          after.isDefined && after.map(v => (v.cards, v.balance)) == before.map(v => (v.cards, v.balance)))
        val start   = player.initialBalance
        val settled = Set(start - Bet, start, start + Bet, start + Bet * 1.5) // lost, push, won, blackjack
        val end     = player.finishHand(seat)
        report.check(s"the hand goes on from where it was and ends on the promoted node: balance $start -> ${end.getOrElse("-")}",
          end.exists(settled.contains))
        report.check(s"${player.name} plays a new hand on the promoted node", player.betAndAwaitCards(seat, Bet))
        seat.connection.close()

  private def crash(): Report =
    val report = Report("The primary crashes")
    freshStart(report)
    playerInAHand(report, "crash").foreach: (player, seat) =>
      val seen = player.settledView(seat)
      report.note(s"before the failure: ${describe(seen)}")
      println("  killing the primary...")
      val granted = Docker.countLog(Backup)(promotionGranted)
      Docker.kill(Primary)
      report.check("the client sees its connection to the primary drop", seat.connection.awaitClosed(Duration.ofSeconds(10)))
      val promoted = Docker.awaitLog(Backup, Reaction, granted)(promotionGranted)
      report.check(s"lobby and backup agree that the primary is gone: the backup is promoted (after ${promoted.fold("-")(seconds)})",
        promoted.isDefined)
      Docker.logLines(Backup).filter(promotionGranted).lastOption.foreach(report.note)
      backOnThePromotedNode(report, player, seen)
    report

  private def partition(): Report =
    val report = Report("The primary is cut off from everybody, then the network comes back")
    freshStart(report)
    playerInAHand(report, "split").foreach: (player, seat) =>
      val seen = player.settledView(seat)
      report.note(s"before the failure: ${describe(seen)}")
      val network = Docker.networkOf(Primary)
      println(s"  disconnecting the primary from $network...")
      val granted = Docker.countLog(Backup)(promotionGranted)
      Docker.isolate(Primary, network)
      val promoted = Docker.awaitLog(Backup, Reaction, granted)(promotionGranted)
      report.check(s"the backup and the lobby are a majority: the backup is promoted (after ${promoted.fold("-")(seconds)})",
        promoted.isDefined)
      backOnThePromotedNode(report, player, seen)
      println("  reconnecting the old primary...")
      val registrations = Docker.countLog(LobbyService)(_.contains(s"registered at localhost:$PrimaryPort"))
      Docker.reconnect(Primary, network)
      val stepped = Docker.awaitLog(Primary, Reaction)(_.contains("steps down"))
      report.check(s"the lobby refuses the old primary, whose epoch is old, and it steps down (after ${stepped.fold("-")(seconds)})",
        stepped.isDefined)
      report.check("its clients are disconnected, so they go back to the lobby", seat.connection.awaitClosed(Duration.ofSeconds(10)))
      report.check("the old primary has not registered again: the players are still sent to the promoted node",
        Docker.countLog(LobbyService)(_.contains(s"registered at localhost:$PrimaryPort")) == registrations)
      Player.signUp(lobby, "late").foreach: late =>
        val address = late.sitDown().map(_.address.wsPort)
        report.check(s"a player who arrives now is sent to the promoted node (port ${address.getOrElse("-")})",
          address == Right(PromotedPort))
    report

  private def backupCutOff(): Report =
    val report = Report("The backup is cut off from everybody, then the network comes back")
    freshStart(report)
    seatedPlayer(report, "alone").foreach: (player, seat) =>
      val network = Docker.networkOf(Backup)
      println(s"  disconnecting the backup from $network...")
      Docker.isolate(Backup, network)
      val refused = player.tryToBet(seat, Bet)
      report.check(s"a move that cannot be replicated is refused, the table is paused (${refused.left.getOrElse("accepted")})",
        refused.isLeft)
      val start = java.time.Instant.now()
      var accepted = refused.isRight
      while !accepted && Duration.between(start, java.time.Instant.now()).compareTo(Reaction) < 0 do
        Thread.sleep(2000)
        accepted = player.tryToBet(seat, Bet).isRight
      report.check(s"the primary and the lobby are a majority: the primary goes on alone and the bet is taken (after ${seconds(Duration.between(start, java.time.Instant.now()))})",
        accepted)
      Docker.logLines(Primary).filter(_.contains("going on without the backup")).lastOption.foreach(report.note)
      println("  reconnecting the backup...")
      Docker.reconnect(Backup, network)
      val synced = Docker.awaitLog(Primary, Reaction)(_.contains("has the whole table again"))
      report.check(s"the backup gets the whole state back and the moves wait for it again (after ${synced.fold("-")(seconds)})",
        synced.isDefined)
      val seen = player.settledView(seat)
      report.note(s"before the failure: ${describe(seen)}")
      println("  killing the primary, to see that the backup is up to date...")
      val granted = Docker.countLog(Backup)(promotionGranted)
      Docker.kill(Primary)
      val promoted = Docker.awaitLog(Backup, Reaction, granted)(promotionGranted)
      report.check(s"the backup, up to date again, takes over (after ${promoted.fold("-")(seconds)})", promoted.isDefined)
      backOnThePromotedNode(report, player, seen)
    report
