package server.auth

import java.security.{MessageDigest, SecureRandom}
import java.util.Base64
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import scala.util.Try

/** Turns a password into what is stored in the database and checks a password against it.
 * Registration and login depend only on this trait. */
trait PasswordHasher:
  /** A salted hash of the password, from which the password cannot be read back. */
  def hash(password: String): String

  /** Whether the password is the one the stored hash was made from. Never throws, a stored value that cannot be read does
   * not match. */
  def verify(password: String, stored: String): Boolean

/** PBKDF2 with HMAC-SHA256, provided by the JDK. Every hash has its own random salt and the HMAC is repeated `iterations`
 * times to slow down guessing. The stored value is `pbkdf2-sha256$<iterations>$<salt>$<hash>` in Base64, so the
 * iterations can be raised without invalidating the registered users.
 *
 * @param iterations how many times the HMAC is applied.
 * @param random     the source of the salts, which must be cryptographically strong.
 */
final class Pbkdf2PasswordHasher(iterations: Int = Pbkdf2PasswordHasher.DefaultIterations,
                                 random: SecureRandom = SecureRandom()) extends PasswordHasher:
  import Pbkdf2PasswordHasher.*
  require(iterations > 0, "iterations must be positive")

  def hash(password: String): String =
    val salt = Array.ofDim[Byte](SaltBytes)
    random.nextBytes(salt)
    val key = derive(password, salt, iterations, KeyBytes)
    List(Prefix, iterations.toString, base64(salt), base64(key)).mkString(Separator)

  def verify(password: String, stored: String): Boolean =
    parse(stored).exists: (storedIterations, salt, expected) =>
      // isEqual takes the same time wherever the first difference is: the time of the answer
      // tells an attacker nothing about how close the guess was (no timing attack)
      MessageDigest.isEqual(derive(password, salt, storedIterations, expected.length), expected)

object Pbkdf2PasswordHasher:
  /** The iterations recommended by OWASP for PBKDF2-HMAC-SHA256. */
  val DefaultIterations = 600_000

  private val Algorithm = "PBKDF2WithHmacSHA256"
  private val Prefix    = "pbkdf2-sha256"
  private val Separator = "$"
  private val SaltBytes = 16
  private val KeyBytes  = 32

  private def derive(password: String, salt: Array[Byte], iterations: Int, bytes: Int): Array[Byte] =
    val spec = PBEKeySpec(password.toCharArray, salt, iterations, bytes * 8)
    try SecretKeyFactory.getInstance(Algorithm).generateSecret(spec).getEncoded
    finally spec.clearPassword() // the copy of the password made by the spec does not stay in memory

  /** Iterations, salt and hash of a stored value, if it is well formed. */
  private def parse(stored: String): Option[(Int, Array[Byte], Array[Byte])] =
    stored.split(java.util.regex.Pattern.quote(Separator)) match
      case Array(Prefix, rawIterations, rawSalt, rawHash) =>
        for
          iterations <- rawIterations.toIntOption.filter(_ > 0)
          salt       <- Try(Base64.getDecoder.decode(rawSalt)).toOption
          hash       <- Try(Base64.getDecoder.decode(rawHash)).toOption.filter(_.nonEmpty)
        yield (iterations, salt, hash)
      case _ => None

  private def base64(bytes: Array[Byte]): String = Base64.getEncoder.encodeToString(bytes)
