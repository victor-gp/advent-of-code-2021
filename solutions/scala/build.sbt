val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Advent of Code 2021 Scala",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
  )
