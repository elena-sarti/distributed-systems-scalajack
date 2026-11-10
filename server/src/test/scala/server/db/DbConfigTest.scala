package server.db

import utils.TestExports.*

class DbConfigTest extends AnyFunSuite:
  private val env = Map("DB_URL" -> "jdbc:postgresql://db:5432/scalajack", "DB_USER" -> "scalajack", "DB_PASSWORD" -> "secret")

  test("reads url, user and password from the environment"):
    DbConfig.fromEnv(env) shouldBe Right(DbConfig("jdbc:postgresql://db:5432/scalajack", "scalajack", "secret"))

  test("fails, naming the variable, when one is missing or empty"):
    Seq("DB_URL", "DB_USER", "DB_PASSWORD").foreach: name =>
      DbConfig.fromEnv(env - name) shouldBe Left(s"$name is not set")
      DbConfig.fromEnv(env.updated(name, "")) shouldBe Left(s"$name is not set")

  test("the password does not appear when the configuration is printed"):
    DbConfig.fromEnv(env).toOption.get.toString should not include "secret"
