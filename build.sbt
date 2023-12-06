import Dependencies._

ThisBuild / scalaVersion     := "3.2.2"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "advant-of-code-2023",
    libraryDependencies += munit % Test,
    libraryDependencies += "org.scala-lang" %% "toolkit" % "0.1.7"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.