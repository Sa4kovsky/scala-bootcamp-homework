import sbt._

object HelloPlugin extends AutoPlugin {
  object autoImport {
    lazy val helloTask = taskKey[Unit]("Prints Hello world.")
  }

  import autoImport._
  override def projectSettings = Seq(
    helloTask := println("Hello world")
  )
}
