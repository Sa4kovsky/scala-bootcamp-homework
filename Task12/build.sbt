name := "Task12"

version := "0.1"

scalaVersion := "2.13.5"

val catsVersion = "2.2.0"
val catsEffectVersion = "2.2.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
)