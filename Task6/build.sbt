import sbt.file

import scala.math.Ordered.orderingToOrdered

name := "Task6"

version := "0.1"

scalaVersion := "2.13.4"

sbtPlugin := true


lazy val bulkyThresholdInLines = settingKey[Int]("number of lines of code to filter")
lazy val bulkySources = taskKey[Seq[(Int, File)]]("implemented for the main and test sources to output 'show bulkySources' showed a list of large sources with the number of rows greater than or equal to the 'bulkyThresholdInLines' setting sorted in descending order of size.")

lazy val root = (project in file("."))
  .settings(
    bulkyThresholdInLines := 100,
    bulkySources := {
      val compileFiles = (Compile / sources).value
      val testCompileFiles = (Test / sources).value
      (compileFiles ++ testCompileFiles)
        .map{ file => (sbt.IO.readLines(file).size, file) }
        .filter{ case (x, _) => x > bulkyThresholdInLines.value }
        .sortWith(_ > _)
    })