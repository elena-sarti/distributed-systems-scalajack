package server.auth

import server.db.{BalanceRepository, CreateUserError, FundsError, SitDownError, User, UserRepository}

/** A repository whose database cannot be reached. */
object BrokenRepository extends UserRepository:
  def create(username: String, passwordHash: String): Either[CreateUserError, User] = unreachable()
  def findByUsername(username: String): Option[User] = unreachable()
  private def unreachable() = throw java.sql.SQLException("Connection refused")

/** Balances whose database cannot be reached. */
object BrokenBalances extends BalanceRepository:
  def balanceOf(username: String): Option[Double] = unreachable()
  def tableOf(username: String): Option[String] = unreachable()
  def sitDown(username: String, tableId: String): Either[SitDownError, Double] = unreachable()
  def standUp(username: String, tableId: String, balance: Double): Boolean = unreachable()
  def deposit(username: String, amount: Double): Either[FundsError, Double] = unreachable()
  def withdraw(username: String, amount: Double): Either[FundsError, Double] = unreachable()
  def releaseTable(tableId: String): Unit = unreachable()
  private def unreachable() = throw java.sql.SQLException("Connection refused")
