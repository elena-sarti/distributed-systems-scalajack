package server.auth

import server.db.{BalanceRepository, CreateUserError, SitDownError, User, UserRepository}

/** The users table in memory, with the same rules as the PostgreSQL one (usernames unique ignoring the
 * case, one table at a time), so the logic that uses it can be tested without a database. */
final class InMemoryUserRepository extends UserRepository, BalanceRepository:
  private final case class Row(user: User, balance: Double, tableId: Option[String])

  private var rows   = Map.empty[String, Row] // by username in lower case, like the database index
  private var nextId = 1L

  def create(username: String, passwordHash: String): Either[CreateUserError, User] = synchronized:
    val key = username.toLowerCase
    if rows.contains(key) then Left(CreateUserError.UsernameTaken)
    else
      val user = User(nextId, username, passwordHash)
      nextId += 1
      rows = rows.updated(key, Row(user, BalanceRepository.InitialBalance, None))
      Right(user)

  def findByUsername(username: String): Option[User] = synchronized(rows.get(username.toLowerCase).map(_.user))

  def balanceOf(username: String): Option[Double] = synchronized(rows.get(username.toLowerCase).map(_.balance))

  def sitDown(username: String, tableId: String): Either[SitDownError, Double] = synchronized:
    rows.get(username.toLowerCase) match
      case None => Left(SitDownError.UnknownUser)
      case Some(Row(_, _, Some(other))) if other != tableId => Left(SitDownError.AlreadySeated(other))
      case Some(row) =>
        rows = rows.updated(username.toLowerCase, row.copy(tableId = Some(tableId)))
        Right(row.balance)

  def standUp(username: String, tableId: String, balance: Double): Boolean = synchronized:
    rows.get(username.toLowerCase) match
      case Some(row) if row.tableId.contains(tableId) =>
        rows = rows.updated(username.toLowerCase, row.copy(balance = balance, tableId = None))
        true
      case _ => false

  def releaseTable(tableId: String): Unit = synchronized:
    rows = rows.map((key, row) => key -> (if row.tableId.contains(tableId) then row.copy(tableId = None) else row))

  /** Every stored user. */
  def all: List[User] = synchronized(rows.values.map(_.user).toList)
