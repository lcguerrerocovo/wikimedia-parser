name := "wikimedia-parser"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies  ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
  "io.circe" %% "circe-generic" % "0.7.0",
  "io.circe" %% "circe-parser" % "0.7.0"
)

// for debugging sbt problems
logLevel := Level.Debug

scalacOptions += "-deprecation"
