package server.auth

import utils.TestExports.*

class Pbkdf2PasswordHasherTest extends AnyFunSuite:
  
  private val hasher   = Pbkdf2PasswordHasher(iterations = 1_000)
  private val password = "correct horse battery staple"

  test("the stored value does not contain the password"):
    hasher.hash(password) should not include password

  test("the right password matches its hash"):
    hasher.verify(password, hasher.hash(password)) shouldBe true

  test("a wrong password does not match"):
    val stored = hasher.hash(password)
    hasher.verify("correct horse battery stapl", stored) shouldBe false
    hasher.verify("", stored) shouldBe false

  test("the same password hashed twice gives two different values, thanks to the salt"):
    val (first, second) = (hasher.hash(password), hasher.hash(password))
    (first != second) shouldBe true
    hasher.verify(password, first) shouldBe true
    hasher.verify(password, second) shouldBe true

  test("the stored value says which algorithm and how many iterations were used"):
    hasher.hash(password) should startWith("pbkdf2-sha256$1000$")

  test("a hash is checked with its own iterations, so they can be raised without breaking old users"):
    val oldHash = Pbkdf2PasswordHasher(iterations = 500).hash(password)
    hasher.verify(password, oldHash) shouldBe true

  test("a stored value that cannot be read does not match, without errors"):
    List("", "plain-password", "pbkdf2-sha256$abc$AAAA$AAAA", "pbkdf2-sha256$1000$not-base64!$AAAA",
         "md5$1000$AAAA$AAAA", "pbkdf2-sha256$1000$AAAA$")
      .foreach(stored => hasher.verify(password, stored) shouldBe false)

  test("by default the iterations follow the OWASP recommendation"):
    Pbkdf2PasswordHasher().hash(password) should startWith(s"pbkdf2-sha256$$${Pbkdf2PasswordHasher.DefaultIterations}$$")
