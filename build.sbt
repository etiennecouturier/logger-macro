lazy val macros_implementations = project in file("macros_implementations")

lazy val root = (project in file("."))
  .settings(
    name := "macros_examples",
    organization := "com.etienne",
    version := "1.0",
    scalaVersion := "2.11.7"
  )
  .enablePlugins(PlayScala)
  .aggregate(macros_implementations)
  .dependsOn(macros_implementations)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)