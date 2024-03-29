libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.6"

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"
/*
name := "akka-quickstart-scala"
version := "1.0"
scalaVersion := "2.12.2"
lazy val akkaVersion = "2.5.3"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
  "org.scalatest" %% "scalatest" % "3.0.1"
)
parallelExecution in Test := false*/