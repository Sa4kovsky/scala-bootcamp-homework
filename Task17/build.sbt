name := "Task17"

version := "0.1"

scalaVersion := "2.13.5"

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-dsl" % "0.21.22",
  "org.http4s" %% "http4s-blaze-server" % "0.21.22",
  "org.http4s" %% "http4s-blaze-client" % "0.21.22",
  "org.http4s" %% "http4s-circe" % "0.21.22",
  "org.http4s" %% "http4s-jdk-http-client" % "0.3.6",
  "io.circe" %% "circe-core" % "0.13.0",
  "io.circe" %% "circe-generic" % "0.13.0",
  "io.circe" %% "circe-generic-extras" % "0.13.0",
  "io.circe" %% "circe-parser" % "0.13.0",
  "org.typelevel" %% "cats-core" % "2.4.2",
  "org.typelevel" %% "cats-effect" % "2.4.1"
)