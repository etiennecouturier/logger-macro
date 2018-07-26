
name := "macros_implementations"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % "2.11.7",
  "org.scalatest" %% "scalatest" % "3.2.0-SNAP10" % Test
)

val paradiseVersion = "2.1.0-M5"

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
