package server.db

import scala.util.Using

/** A repository on the `users` table of PostgreSQL. */
final class PostgresUserRepository(database: Database) extends UserRepository:

  // ON CONFLICT DO NOTHING: if the unique index rejects the row nothing is inserted and nothing is returned.
  // Insert and uniqueness check are one atomic statement, so there is no "check, then insert" race.
  private val InsertSql =
    "INSERT INTO users (username, password_hash) VALUES (?, ?) ON CONFLICT DO NOTHING RETURNING id"

  // lower(...) on both sides matches the index on lower(username), so the lookup uses it.
  private val SelectSql =
    "SELECT id, username, password_hash FROM users WHERE lower(username) = lower(?)"

  def create(username: String, passwordHash: String): Either[CreateUserError, User] =
    database.withConnection: connection =>
      Using.resource(connection.prepareStatement(InsertSql)): statement =>
        statement.setString(1, username) // parameters are sent apart from the SQL: no SQL injection
        statement.setString(2, passwordHash)
        Using.resource(statement.executeQuery()): rows =>
          if rows.next() then Right(User(rows.getLong("id"), username, passwordHash))
          else Left(CreateUserError.UsernameTaken)

  def findByUsername(username: String): Option[User] =
    database.withConnection: connection =>
      Using.resource(connection.prepareStatement(SelectSql)): statement =>
        statement.setString(1, username)
        Using.resource(statement.executeQuery()): rows =>
          Option.when(rows.next())(User(rows.getLong("id"), rows.getString("username"), rows.getString("password_hash")))
