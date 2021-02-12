package Result

import Command.Command

sealed trait Result

final case class ChangeMe(command: Command, result: Double) extends Result