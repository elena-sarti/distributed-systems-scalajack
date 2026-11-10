package server

import server.db.{BalanceRepository, SitDownError}
import scala.util.control.NonFatal

/** Where a table takes the money of a player who sits down, and gives it back when the player leaves. */
trait Cashier:
  /** The money the player brings to the table, or why they cannot sit down. */
  def sitDown(username: String): Either[String, Double]

  /** Gives back the money the player has when leaving the table. */
  def standUp(username: String, balance: Double): Unit

object Cashier:
  /** Everybody sits down with the same balance and nothing is kept. */
  def withoutMemory(balance: Double): Cashier = new Cashier:
    def sitDown(username: String): Either[String, Double] = Right(balance)
    def standUp(username: String, balance: Double): Unit  = ()

/** The balances kept in the database: a player finds at the next table, and at the next login, the balance they had when
 * they left.
 * It is called by the single writer, so a slow database slows the table down. Errors only become a refusal or a line in
 * the log. */
final class DatabaseCashier(balances: BalanceRepository, tableId: String) extends Cashier:
  import DatabaseCashier.*

  def sitDown(username: String): Either[String, Double] =
    try balances.sitDown(username, tableId).left.map:
      case SitDownError.UnknownUser      => UnknownUser
      case SitDownError.AlreadySeated(_) => SeatedElsewhere
    catch case NonFatal(error) =>
      System.err.println(s"Cannot load the balance of $username: ${error.getMessage}")
      Left(Unavailable)

  def standUp(username: String, balance: Double): Unit =
    try
      if !balances.standUp(username, tableId, balance) then
        System.err.println(s"The balance of $username was not saved: not sitting at $tableId")
    catch case NonFatal(error) =>
      // the user stays marked at this table until it starts again
      System.err.println(s"Cannot save the balance of $username: ${error.getMessage}")

object DatabaseCashier:
  val UnknownUser     = "This user does not exist"
  val SeatedElsewhere = "You are already sitting at another table"
  val Unavailable     = "Your balance cannot be loaded now, try again later"
