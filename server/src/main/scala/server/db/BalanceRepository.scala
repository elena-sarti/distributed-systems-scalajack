package server.db

/** Why a user cannot sit down at a table. */
enum SitDownError:
  case UnknownUser
  /** The user is already sitting at another table. */
  case AlreadySeated(tableId: String)

/** The balances of the users, kept between sessions, and the table where each user is sitting.
 *
 * A table takes the user's balance when they sit down and gives it back when they leave. In between
 * nobody else can take it: a user sits at one table at a time. The database guarantees this with
 * conditional updates, which it runs atomically, so two tables that try at the same moment cannot
 * both succeed.
 */
trait BalanceRepository:

  /** The user's balance, if the user exists. */
  def balanceOf(username: String): Option[Double]

  /** Marks the user as sitting at the table and returns their balance. Sitting down again at the same
   * table is allowed (e.g. after reconnecting); at any other table it is refused. */
  def sitDown(username: String, tableId: String): Either[SitDownError, Double]

  /** Saves the balance and frees the user, if they are sitting at this table.
   *
   * @return whether the balance was saved (false if the user was not sitting at this table).
   */
  def standUp(username: String, tableId: String, balance: Double): Boolean

  /** Frees every user still marked as sitting at the table: called when the table starts, because after a
   * crash nobody is really sitting there any more. */
  def releaseTable(tableId: String): Unit

object BalanceRepository:
  /** The balance of a new user (the default of the `balance` column in `db/schema.sql`). */
  val InitialBalance = 100.0
