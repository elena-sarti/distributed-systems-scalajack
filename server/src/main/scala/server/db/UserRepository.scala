package server.db

/** A registered player. `passwordHash` is the stored hash, never the password itself. */
final case class User(id: Long, username: String, passwordHash: String)

enum CreateUserError:
  /** A user with this username already exists (usernames are compared ignoring the case). */
  case UsernameTaken

/** Where the users are stored. Registration and login depend on this trait and not on PostgreSQL (DIP), so the SQL stays
 * in one class. */
trait UserRepository:
  /** Stores a new user, or says that the username is already taken. Of two simultaneous requests for the same username
   * exactly one succeeds. */
  def create(username: String, passwordHash: String): Either[CreateUserError, User]

  /** The user with this username (ignoring the case), if there is one. */
  def findByUsername(username: String): Option[User]
