package server.auth

import server.db.{CreateUserError, User, UserRepository}

/** A [[UserRepository]] in memory, with the same rules as the PostgreSQL one (usernames unique ignoring
 * the case), so the logic that uses it can be tested without a database. */
final class InMemoryUserRepository extends UserRepository:
  private var users  = Map.empty[String, User] // by username in lower case, like the database index
  private var nextId = 1L

  def create(username: String, passwordHash: String): Either[CreateUserError, User] = synchronized:
    val key = username.toLowerCase
    if users.contains(key) then Left(CreateUserError.UsernameTaken)
    else
      val user = User(nextId, username, passwordHash)
      nextId += 1
      users = users.updated(key, user)
      Right(user)

  def findByUsername(username: String): Option[User] = synchronized(users.get(username.toLowerCase))

  /** Every stored user. */
  def all: List[User] = synchronized(users.values.toList)
