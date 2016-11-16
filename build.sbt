name := "CSound++"

version := "0.0.1"

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
)

unmanagedClasspath in (Compile, runMain) += baseDirectory.value / "resources"
