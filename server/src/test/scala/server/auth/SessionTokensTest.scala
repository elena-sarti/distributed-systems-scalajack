package server.auth

import utils.TestExports.*
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant
import java.util.Base64
import scala.concurrent.duration.*

class SessionTokensTest extends AnyFunSuite:

  private val secret = "a test secret that is long enough for HMAC".getBytes(UTF_8)
  private var now    = Instant.parse("2026-11-01T18:00:00Z")
  private val tokens = SessionTokens(secret, validity = 1.hour, clock = () => now)

  test("a token issued for a user is accepted and tells who the user is"):
    tokens.verify(tokens.issue("Alice")) shouldBe Some("Alice")

  test("any username survives the trip, dots and accents included"):
    List("Alice.Smith", "Zoë", "名前").foreach(name => tokens.verify(tokens.issue(name)) shouldBe Some(name))

  test("a token signed with another secret is refused"):
    val other = SessionTokens("another secret, just as long as the first one".getBytes(UTF_8), clock = () => now)
    tokens.verify(other.issue("Alice")) shouldBe None

  test("changing the name inside a token makes it invalid"):
    val Array(_, expiry, signature) = tokens.issue("Bob").split('.'): @unchecked
    val alice = Base64.getUrlEncoder.withoutPadding.encodeToString("Alice".getBytes(UTF_8))
    tokens.verify(s"$alice.$expiry.$signature") shouldBe None

  test("extending the expiry inside a token makes it invalid"):
    val Array(user, expiry, signature) = tokens.issue("Alice").split('.'): @unchecked
    tokens.verify(s"$user.${expiry.toLong + 3600}.$signature") shouldBe None

  test("a token is valid until it expires, and not a moment after"):
    val token = tokens.issue("Alice")
    now = now.plusSeconds(3599)
    tokens.verify(token) shouldBe Some("Alice")
    now = now.plusSeconds(1)
    tokens.verify(token) shouldBe None

  test("anything that is not a token is refused without errors"):
    List("", "abc", "a.b", "a.b.c.d", "!!.123.!!", "QWxpY2U.notanumber.c2ln").foreach: token =>
      tokens.verify(token) shouldBe None

  test("the secret is read from SESSION_SECRET and must be long enough"):
    SessionTokens.fromEnv(Map.empty) shouldBe Left("SESSION_SECRET is not set")
    SessionTokens.fromEnv(Map("SESSION_SECRET" -> "too short")).isLeft shouldBe true
    SessionTokens.fromEnv(Map("SESSION_SECRET" -> "x" * SessionTokens.MinSecretBytes)).isRight shouldBe true
