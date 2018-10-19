lazy val macros_implementations = project in file("macros_implementations")

lazy val root = (project in file("."))
  .settings(
    name := "macros_examples",
    organization := "com.etienne",
    version := "1.0",
    scalaVersion := "2.11.7"
  )
  .aggregate(macros_implementations)
  .dependsOn(macros_implementations)
//  .enablePlugins(PlayScala)

val paradiseVersion = "2.1.0-M5"
addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)