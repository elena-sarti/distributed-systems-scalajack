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
  .settings(name := "scalajack-server",
    commonTestDeps,
    Compile / mainClass := Some("server.Main"),
    assembly / assemblyJarName := "scalajack-server.jar"
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
