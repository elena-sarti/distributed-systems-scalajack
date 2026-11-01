package server.auth

import server.db.{CreateUserError, User, UserRepository}
import utils.ModelExports.Game

/** Why a registration was refused. */
enum RegistrationError:
  /** The username breaks the rules (see [[RegistrationService.isUsernameValid]]). */
  case InvalidUsername
  /** The password is too short or too long (see [[RegistrationService.isPasswordValid]]). */
  case InvalidPassword
  /** Somebody already registered this username (ignoring the case). */
  case UsernameTaken

/** Registers new users: checks username and password, then stores the user with the *hash* of the password.
 *
 * The password is only handed to the hasher: it never reaches the repository, so it cannot end up in the
 * database, nor in a database error. The service writes nothing to the logs.
 * Thread-safe: it keeps no state of its own, and the uniqueness of the username is guaranteed by the
 * repository (in PostgreSQL, by a unique index), even when two requests arrive at the same moment.
 */
final class RegistrationService(users: UserRepository, hasher: PasswordHasher):
  import RegistrationService.*

  def register(username: String, password: String): Either[RegistrationError, User] =
    if !isUsernameValid(username) then Left(RegistrationError.InvalidUsername)
    else if !isPasswordValid(password) then Left(RegistrationError.InvalidPassword)
    else users.create(username, hasher.hash(password)).left.map:
      case CreateUserError.UsernameTaken => RegistrationError.UsernameTaken

object RegistrationService:
  /** The size of the `username` column (see `db/schema.sql`). */
  val MaxUsernameLength = 32
  /** At least 8 characters, as recommended by NIST SP 800-63B. */
  val MinPasswordLength = 8
  /** An upper limit, so that a huge input cannot keep the hasher busy. */
  val MaxPasswordLength = 128

  /** The username is the name the player has at the tables, so it follows the same rules as there
   * (not empty, no '_', which the model reserves for split hands), it fits the column, and it has no
   * spaces around it ("Alice " would look like "Alice" to the other players). */
  def isUsernameValid(username: String): Boolean =
    Game.arePlayersNamesValid(1)(List(username)) &&
      username.length <= MaxUsernameLength &&
      username == username.strip

  def isPasswordValid(password: String): Boolean =
    password.length >= MinPasswordLength && password.length <= MaxPasswordLength
