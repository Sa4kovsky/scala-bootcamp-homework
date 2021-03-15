name := "Task10"

version := "0.1"

scalaVersion := "2.13.5"

val circeVersion = "0.13.0"
val scalaTestVersion = "3.1.0.0-RC2"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-optics" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.scalaj" %% "scalaj-http" % "2.4.2" % Test,
  "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test
)