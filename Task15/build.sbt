name := "Task15"

version := "0.1"

scalaVersion := "2.13.5"

val circeVersion = "0.13.0"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.typelevel" %% "cats-core" % "2.3.1",
  "org.typelevel" %% "cats-effect" % "2.4.1",
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-optics" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion
)