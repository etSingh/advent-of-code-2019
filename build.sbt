import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "adventOfCode-2019",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "com.softwaremill.sttp.client" %% "core" % "2.0.0-RC3",
    libraryDependencies += "com.typesafe" % "config" % "1.4.0"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
