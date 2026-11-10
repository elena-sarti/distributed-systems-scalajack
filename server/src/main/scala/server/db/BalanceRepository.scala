package server.db

/** Why a user cannot sit down at a table. */
enum SitDownError:
  case UnknownUser
  /** The user is already sitting at another table. */
  case AlreadySeated(tableId: String)

/** Why money cannot be deposited or withdrawn. */
enum FundsError:
  case UnknownUser
  /** The user is sitting at a table, which holds their money until they stand up. */
  case Seated(tableId: String)
  /** The balance is lower than the amount to withdraw. */
  case InsufficientFunds

/** The balances of the users, kept between sessions, and the table where each user sits. A table takes the balance when
 * the user sits down and gives it back when they leave. The database guarantees with atomic conditional updates that a
 * user sits at one table at a time. */
trait BalanceRepository:

  /** The user's balance, if the user exists. */
  def balanceOf(username: String): Option[Double]

  /** The table where the user is sitting, or `None` if they sit nowhere or do not exist. */
  def tableOf(username: String): Option[String]

  /** Marks the user as sitting at the table and returns their balance. Sitting down again at the same table is allowed, at
   * any other table it is refused. */
  def sitDown(username: String, tableId: String): Either[SitDownError, Double]

  /** Saves the balance and frees the user, if they are sitting at this table.
   *
   * @return whether the balance was saved, which is not the case if the user was not sitting at this table.
   */
  def standUp(username: String, tableId: String, balance: Double): Boolean

  /** Adds a positive `amount` to the balance of a user who is not sitting at a table and returns the new balance, in one
   * atomic statement. */
  def deposit(username: String, amount: Double): Either[FundsError, Double]

  /** Takes a positive `amount` from the balance of a user who is not sitting at a table and returns the new balance. It is
   * refused if the balance is lower, so it never goes below zero. */
  def withdraw(username: String, amount: Double): Either[FundsError, Double]

  /** Frees every user still marked as sitting at the table. Called when the table starts, since after a crash nobody sits
   * there anymore. */
  def releaseTable(tableId: String): Unit

object BalanceRepository:
  /** The balance of a new user, as in the `balance` column of `db/schema.sql`, so the first thing a player does is a deposit. */
  val InitialBalance = 0.0
