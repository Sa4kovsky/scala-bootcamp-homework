package Result

sealed trait Result

final case class ChangeMe(value: String) extends Result
