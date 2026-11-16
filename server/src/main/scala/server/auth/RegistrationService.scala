package server.auth

import server.db.{CreateUserError, User, UserRepository}
import utils.ModelExports.Game

/** Why a registration was refused. */
enum RegistrationError:
  /** The username breaks the rules. */
  case InvalidUsername
  /** The password is too short or too long. */
  case InvalidPassword
  /** Somebody already registered this username. */
  case UsernameTaken

/** Registers new users. It checks username and password and stores the user with the *hash* of the password, which is the
 * only thing that reaches the repository. It writes nothing to the logs and is thread-safe, since the repository
 * guarantees the uniqueness of the username. */
final class RegistrationService(users: UserRepository, hasher: PasswordHasher):
  import RegistrationService.*

  def register(username: String, password: String): Either[RegistrationError, User] =
    if !isUsernameValid(username) then Left(RegistrationError.InvalidUsername)
    else if !isPasswordValid(password) then Left(RegistrationError.InvalidPassword)
    else users.create(username, hasher.hash(password)).left.map:
      case CreateUserError.UsernameTaken => RegistrationError.UsernameTaken

object RegistrationService:
  /** The size of the `username` column. */
  val MaxUsernameLength = 32
  /** At least 8 characters. */
  val MinPasswordLength = 8
  /** An upper limit, so that a huge input cannot keep the hasher busy. */
  val MaxPasswordLength = 128

  /** Whether the username follows the rules of the names at the tables, so it is not empty and has no '_', which the model
   * reserves for split hands. It must also fit the column and have no spaces around it. */
  def isUsernameValid(username: String): Boolean =
    Game.arePlayersNamesValid(1)(List(username)) &&
      username.length <= MaxUsernameLength &&
      username == username.strip

  def isPasswordValid(password: String): Boolean =
    password.length >= MinPasswordLength && password.length <= MaxPasswordLength
