import sbt._

object HelloPlugin extends AutoPlugin {

  lazy val helloTask = taskKey[Unit]("Prints Hello world.")

  override def projectSettings = Seq(
    helloTask := println("Hello world")
  )
}
