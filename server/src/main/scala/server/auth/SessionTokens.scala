package server.auth

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.time.Instant
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import scala.concurrent.duration.*
import scala.util.Try

/** Session tokens, issued by the lobby at login and checked by the tables. A token carries the username and an expiry,
 * signed with HMAC-SHA256 with a secret shared by lobby and tables, so a table checks it without asking anyone.
 * Tokens cannot be revoked and stay valid until they expire.
 *
 * Format: `<username, Base64>.<expiry, epoch seconds>.<signature, Base64>`. */
final class SessionTokens(secret: Array[Byte],
                          validity: FiniteDuration = SessionTokens.DefaultValidity,
                          clock: () => Instant = () => Instant.now()):
  import SessionTokens.*
  require(secret.length >= MinSecretBytes, s"The secret must be at least $MinSecretBytes bytes long")

  /** A signed token for the user, valid for the configured time. */
  def issue(username: String): String =
    val payload = s"${encode(username.getBytes(UTF_8))}.${clock().plusSeconds(validity.toSeconds).getEpochSecond}"
    s"$payload.${encode(sign(payload))}"

  /** The username in the token, if the token is genuine and not expired. */
  def verify(token: String): Option[String] =
    token.split('.') match
      case Array(user, expiry, signature) =>
        val payload = s"$user.$expiry"
        for
          received  <- decode(signature)
          if MessageDigest.isEqual(received, sign(payload)) // constant time, like the password check
          expiresAt <- expiry.toLongOption
          if clock().getEpochSecond < expiresAt
          username  <- decode(user).map(String(_, UTF_8))
        yield username
      case _ => None

  private def sign(payload: String): Array[Byte] =
    val mac = Mac.getInstance(Algorithm)
    mac.init(SecretKeySpec(secret, Algorithm))
    mac.doFinal(payload.getBytes(UTF_8))

object SessionTokens:
  val DefaultValidity: FiniteDuration = 12.hours
  val MinSecretBytes = 32 // as long as the HMAC-SHA256 output: a shorter key is easier to guess

  private val Algorithm = "HmacSHA256"

  /** Reads the shared secret from `SESSION_SECRET`. Lobby and tables must have the same value. */
  def fromEnv(env: Map[String, String]): Either[String, SessionTokens] =
    env.get("SESSION_SECRET").filter(_.nonEmpty).toRight("SESSION_SECRET is not set").flatMap: secret =>
      val bytes = secret.getBytes(UTF_8)
      Either.cond(bytes.length >= MinSecretBytes, SessionTokens(bytes),
        s"SESSION_SECRET must be at least $MinSecretBytes characters long")

  // URL-safe and without padding, so the token contains only letters, digits, '-', '_' and the two dots
  private def encode(bytes: Array[Byte]): String = Base64.getUrlEncoder.withoutPadding.encodeToString(bytes)
  private def decode(text: String): Option[Array[Byte]] = Try(Base64.getUrlDecoder.decode(text)).toOption
