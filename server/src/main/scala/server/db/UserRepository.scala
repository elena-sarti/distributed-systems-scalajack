package server.db

/** A registered player. `passwordHash` is the stored hash, never the password itself. */
final case class User(id: Long, username: String, passwordHash: String)

enum CreateUserError:
  /** A user with this username already exists (usernames are compared ignoring the case). */
  case UsernameTaken

/** Where the users are stored.
 *
 * Registration (D2) and login (D3) depend on this trait and not on PostgreSQL (Dependency Inversion):
 * their tests can use an in-memory implementation, and the SQL stays in one class. */
trait UserRepository:
  /** Stores a new user, or says that the username is already taken. Safe with concurrent calls:
   * if two requests register the same username at the same moment, exactly one of them succeeds. */
  def create(username: String, passwordHash: String): Either[CreateUserError, User]

  /** The user with this username (ignoring the case), if there is one. */
  def findByUsername(username: String): Option[User]
