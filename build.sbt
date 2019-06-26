name := "asm-lambda"

ThisBuild / version := "0.1"

ThisBuild / scalaVersion := "2.12.8"

ThisBuild / scalacOptions ++= Seq(
  "-feature", // doesn't allow advance features of the language without explict import (higherkinds, implicits)
  "-encoding", "UTF-8",
  "-unchecked", // more detailed information about type-erasure related warnings
  "-deprecation", // warn if using deprecated stuff
  "-Xfuture",
  "-Xlint:_,-unused",
  // "-Xfatal-warnings", TODO re-enable
  "-Yno-adapted-args", // adapted args is a deprecated feature: `def foo(a: (A, B))` can be called with `foo(a, b)`. properly it should be `foo((a,b))`
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen", // Warn about implicit conversion between numerical types
  "-Ywarn-value-discard", // Gives a warning for functions declared as returning Unit, but the body returns a value
  "-Ywarn-unused-import",
  "-Ywarn-unused",
)

ThisBuild / javacOptions ++= Seq("-source", "10")

lazy val runtime = project in file("runtime")

lazy val compiler = (project in file("compiler"))
    .settings(
      libraryDependencies += "org.ow2.asm" % "asm" % "7.0",

      libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test",
      libraryDependencies += "org.ow2.asm" % "asm-util" % "7.0" % "test")
    .dependsOn(runtime)

lazy val bench = (project in file("bench"))
    .dependsOn(runtime, compiler)
    .enablePlugins(JmhPlugin)
