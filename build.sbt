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
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "commons-lang" % "commons-lang" % "2.6",
  "com.github.scopt" %% "scopt" % "3.2.0",
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3"
)

unmanagedClasspath in (Compile, runMain) += baseDirectory.value / "resources"

assemblyOutputPath in assembly := baseDirectory.value / "bin" / "cspp.jar"
