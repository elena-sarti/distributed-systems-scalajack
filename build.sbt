ThisBuild / version      := "1.0.0"
ThisBuild / scalaVersion := "3.3.5"

lazy val commonTestDeps = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.18" % Test
  )
)

lazy val model = project
  .settings(
    name := "scalajack-model",
    commonTestDeps,
    libraryDependencies += "it.unibo.alice.tuprolog" % "tuprolog" % "3.3.0"
  )

lazy val protocol = project
  .dependsOn(model)
  .settings(
    name := "scalajack-protocol",
    commonTestDeps,
    // upickle: JSON (de)serialization of the protocol messages, derived at compile time
    libraryDependencies += "com.lihaoyi" %% "upickle" % "4.1.0"
  )

lazy val server = project
  .dependsOn(protocol)
  .settings(
    name := "scalajack-server",
    commonTestDeps,
    libraryDependencies ++= Seq(
      "org.java-websocket" % "Java-WebSocket" % "1.6.0",
      "org.slf4j"          % "slf4j-simple"   % "2.0.13", // log backend, otherwise the library's logs are dropped
      "org.postgresql"     % "postgresql"     % "42.7.4", // JDBC driver: how the lobby talks to PostgreSQL
      // a real PostgreSQL started inside the test JVM, so the database tests need neither Docker nor a manual setup
      "io.zonky.test"      % "embedded-postgres" % "2.0.7" % Test
    ),
    Compile / mainClass := Some("server.Main"),
    assembly / assemblyJarName := "scalajack-server.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "services", _*) => MergeStrategy.concat // keeps the SLF4J provider registration
      case PathList("META-INF", _*)             => MergeStrategy.discard
      case "module-info.class"                  => MergeStrategy.discard
      case _                                    => MergeStrategy.first
    }
  )

lazy val client = project
  .dependsOn(protocol)
  .settings(
    name := "scalajack-client",
    commonTestDeps,
    libraryDependencies ++= Seq(
      "org.typelevel"      %% "cats-effect"    % "3.5.4",
      "org.java-websocket" %  "Java-WebSocket" % "1.6.0"  % Test, // fake table server for the network tests
      "org.slf4j"          %  "slf4j-simple"   % "2.0.13" % Test
    ),
    Compile / mainClass := Some("controller.OnlineGame"), // the local game of the first version: controller.GameController
    run / fork := true,              // the game reads the keyboard: in its own JVM, connected to the terminal
    run / connectInput := true,
    outputStrategy := Some(StdoutOutput),
    assembly / assemblyOutputPath := file("./scalajack-client.jar")
  )

// The failures of the decision record played on the deployment of docker-compose.yml: `sbt "demo/run"`
lazy val demo = project
  .dependsOn(protocol)
  .settings(
    name := "scalajack-demo",
    Compile / mainClass := Some("demo.FailureDemo"),
    run / fork := true, // the demo ends with an exit code, which must not close sbt
    run / baseDirectory := (ThisBuild / baseDirectory).value // docker compose is run from the root of the repository
  )

lazy val root = (project in file("."))
  .aggregate(model, protocol, server, client, demo)
  .settings(name := "scalajack", publish / skip := true)
