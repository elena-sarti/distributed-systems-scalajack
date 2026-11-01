package server.auth

import server.db.{CreateUserError, User, UserRepository}

/** A repository whose database cannot be reached. */
object BrokenRepository extends UserRepository:
  def create(username: String, passwordHash: String): Either[CreateUserError, User] = unreachable()
  def findByUsername(username: String): Option[User] = unreachable()
  private def unreachable() = throw java.sql.SQLException("Connection refused")
