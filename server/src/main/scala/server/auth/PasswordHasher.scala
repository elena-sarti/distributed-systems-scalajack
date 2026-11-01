package server.auth

import java.security.{MessageDigest, SecureRandom}
import java.util.Base64
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import scala.util.Try

/** Turns a password into what is stored in the database, and checks a password against it.
 * Registration and login only know this trait, so their tests can use a fast hasher. */
trait PasswordHasher:
  /** A salted hash of the password: the password itself cannot be read back from it. */
  def hash(password: String): String

  /** Whether the password is the one the stored hash was made from. Never throws: a stored value
   * that cannot be read simply does not match. */
  def verify(password: String, stored: String): Boolean

/** PBKDF2 with HMAC-SHA256, from the JDK (no extra dependency).
 *
 *  - Salt: every hash gets its own random salt, so two users with the same password get different
 *    hashes, and precomputed tables of hashes (rainbow tables) are useless.
 *  - Slow on purpose: the HMAC is repeated `iterations` times, so whoever steals the database needs
 *    a lot of time for every password guessed.
 *  - Self-describing: the stored value is `pbkdf2-sha256$<iterations>$<salt>$<hash>` (Base64), so a hash
 *    is always checked with the parameters it was made with, and the iterations can be raised later
 *    without invalidating the users already registered.
 *
 * @param iterations how many times the HMAC is applied (tests use few, to be fast).
 * @param random     source of the salts; it must be cryptographically strong.
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
  /** OWASP recommendation for PBKDF2-HMAC-SHA256 (Password Storage Cheat Sheet). */
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
