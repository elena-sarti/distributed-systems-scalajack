package server.auth

import server.db.UserRepository

/** A logged-in user: the username as it was registered, and the token to show to the tables.
 * `toString` hides the token, which works like a password until it expires. */
final case class Session(username: String, token: String):
  override def toString: String = s"Session($username,***)"

/** Checks username and password and, if they are right, opens a session. */
final class LoginService(users: UserRepository, hasher: PasswordHasher, tokens: SessionTokens):

  // Checked when the username does not exist, so that an unknown user takes as long as a wrong
  // password: the time of the answer does not tell who is registered.
  private lazy val dummyHash = hasher.hash("no user has this password")

  /** The new session, or None if the username or the password is wrong (the caller is not told which). */
  def login(username: String, password: String): Option[Session] =
    users.findByUsername(username) match
      case Some(user) if hasher.verify(password, user.passwordHash) =>
        Some(Session(user.username, tokens.issue(user.username)))
      case Some(_) => None
      case None =>
        hasher.verify(password, dummyHash)
        None
