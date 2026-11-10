package server.db

import java.math.BigDecimal
import java.sql.{Connection, PreparedStatement, ResultSet}
import scala.util.Using

/** [[BalanceRepository]] on the `balance` and `table_id` columns of the `users` table. */
final class PostgresBalanceRepository(database: Database) extends BalanceRepository:

  // The WHERE clause is the lock: if another table has already set table_id, no row matches and
  // nothing changes. PostgreSQL runs the check and the update as one step.
  private val SitDownSql =
    """UPDATE users SET table_id = ?
      |WHERE lower(username) = lower(?) AND (table_id IS NULL OR table_id = ?)
      |RETURNING balance""".stripMargin

  private val StandUpSql =
    "UPDATE users SET balance = ?, table_id = NULL WHERE lower(username) = lower(?) AND table_id = ?"

  // One statement for both directions: a deposit adds a positive `delta`, a withdrawal a negative one. The WHERE
  // clause is the lock again (nobody is sitting) and also the guard of the balance (it cannot become negative).
  private val ChangeSql =
    """UPDATE users SET balance = balance + ?
      |WHERE lower(username) = lower(?) AND table_id IS NULL AND balance + ? >= 0
      |RETURNING balance""".stripMargin

  private val ReleaseSql = "UPDATE users SET table_id = NULL WHERE table_id = ?"

  private val SelectSql = "SELECT balance, table_id FROM users WHERE lower(username) = lower(?)"

  def balanceOf(username: String): Option[Double] =
    select(username)(rows => rows.getBigDecimal("balance").doubleValue)

  def tableOf(username: String): Option[String] =
    select(username)(rows => Option(rows.getString("table_id"))).flatten

  def sitDown(username: String, tableId: String): Either[SitDownError, Double] =
    val balance = withStatement(SitDownSql): statement =>
      statement.setString(1, tableId)
      statement.setString(2, username)
      statement.setString(3, tableId)
      Using.resource(statement.executeQuery())(rows => Option.when(rows.next())(rows.getBigDecimal("balance").doubleValue))
    balance match
      case Some(found) => Right(found)
      case None => select(username)(rows => Option(rows.getString("table_id"))) match
        case None                    => Left(SitDownError.UnknownUser)
        case Some(Some(otherTable))  => Left(SitDownError.AlreadySeated(otherTable))
        case Some(None)              => sitDown(username, tableId) // the other table has just freed the user

  def standUp(username: String, tableId: String, balance: Double): Boolean =
    withStatement(StandUpSql): statement =>
      statement.setBigDecimal(1, BigDecimal.valueOf(balance))
      statement.setString(2, username)
      statement.setString(3, tableId)
      statement.executeUpdate() > 0

  def deposit(username: String, amount: Double): Either[FundsError, Double] = change(username, amount)

  def withdraw(username: String, amount: Double): Either[FundsError, Double] = change(username, -amount)

  /** Applies `delta` to the balance, or finds out why it cannot be done. */
  private def change(username: String, delta: Double): Either[FundsError, Double] =
    val exactDelta = BigDecimal.valueOf(delta)
    val balance = withStatement(ChangeSql): statement =>
      statement.setBigDecimal(1, exactDelta)
      statement.setString(2, username)
      statement.setBigDecimal(3, exactDelta)
      Using.resource(statement.executeQuery())(rows => Option.when(rows.next())(rows.getBigDecimal("balance").doubleValue))
    balance match
      case Some(found) => Right(found)
      case None => select(username)(rows => (rows.getBigDecimal("balance"), Option(rows.getString("table_id")))) match
        case None                                    => Left(FundsError.UnknownUser)
        case Some((_, Some(table)))                  => Left(FundsError.Seated(table))
        // the same exact arithmetic of the database: no rounding that could disagree with it
        case Some((current, None)) if current.add(exactDelta).signum < 0 => Left(FundsError.InsufficientFunds)
        case Some((_, None))                         => change(username, delta) // the user was sitting and has just stood up

  def releaseTable(tableId: String): Unit =
    withStatement(ReleaseSql): statement =>
      statement.setString(1, tableId)
      statement.executeUpdate()

  private def select[A](username: String)(read: ResultSet => A): Option[A] =
    withStatement(SelectSql): statement =>
      statement.setString(1, username)
      Using.resource(statement.executeQuery())(rows => Option.when(rows.next())(read(rows)))

  private def withStatement[A](sql: String)(use: PreparedStatement => A): A =
    database.withConnection((connection: Connection) => Using.resource(connection.prepareStatement(sql))(use))
