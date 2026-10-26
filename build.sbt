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
      "org.slf4j"          % "slf4j-simple"   % "2.0.13" // log backend, otherwise the library's logs are dropped
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
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.4",
    Compile / mainClass := Some("controller.GameController"),
    assembly / assemblyOutputPath := file("./scalajack-client.jar")
  )

lazy val root = (project in file("."))
  .aggregate(model, protocol, server, client)
  .settings(name := "scalajack", publish / skip := true)
