name := "Task1"

version := "0.1"

scalaVersion := "2.13.4"

val scalaTestVersion = "3.1.0.0-RC2"

libraryDependencies ++= Seq(
  "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test
)

lazy val myLibraryinGit = RootProject(
  uri(
    "https://github.com/Sa4kovsky/scala-bootcamp-homework.git#fbac26c9a9c6f9aa2f2d358d228742bb0dbd8a0d"
  )
)

lazy val root = (project in file(".")).dependsOn(myLibraryinGit)
