name := "Task1"

version := "0.1"

scalaVersion := "2.13.4"

val scalaTestVersion = "3.1.0.0-RC2"

libraryDependencies ++= Seq(
  "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test
)

lazy val myLibraryinGit = RootProject(
  uri(
    "http://github.com/ground5hark/sbt-concat.git"
    "https://github.com/Sa4kovsky/scala-bootcamp-homework.git#0e8dc483cff35205d591109fbbe0c19fe2c40e72"
  )
)

lazy val root = (project in file(".")).dependsOn(myLibraryinGit)
