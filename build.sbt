val SCALA_VERSION = "2.11.12"

organization := "com.factual.reservoir"
publishMavenStyle := true
scalaVersion := SCALA_VERSION

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "0.7.5" % Test
)

testFrameworks += new TestFramework("munit.Framework")
