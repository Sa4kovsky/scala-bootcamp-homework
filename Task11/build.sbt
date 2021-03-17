name := "Task11"

version := "0.1"

scalaVersion := "2.13.5"

val scalaTestVersion = "3.1.0.0-RC2"

libraryDependencies ++= Seq(
  "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test,
  "org.scalatestplus" %% "selenium-2-45" % scalaTestVersion % Test
)

resolvers += Resolver.url("scoverage-bintray", url("https://dl.bintray.com/sksamuel/sbt-plugins/"))