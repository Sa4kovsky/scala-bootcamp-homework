import Command.Command
import Error.ErrorMessage
import Result.Result

import scala.io.Source

object Main {

  def parseCommand(line: String): Either[ErrorMessage, Command] = {
    line.toLowerCase.split("\\s+").toList match {
      case "divide" :: dividend :: divisor :: Nil  => Right(Command.Divide(dividend.toDouble, divisor.toDouble))
      case "divide" :: _                           => Left(ErrorMessage("Invalid input"))
      case "sum" :: t if t.nonEmpty                => Right(Command.Sum(t.map(x => x.toDouble)))
      case "sum" :: _                              => Left(ErrorMessage("Invalid input"))
      case "average" :: t if t.nonEmpty            => Right(Command.Average(t.map(x => x.toDouble)))
      case "average" :: _                          => Left(ErrorMessage("Invalid input"))
      case "min" :: t if t.nonEmpty                => Right(Command.Min(t.map(x => x.toDouble)))
      case "min" :: _                              => Left(ErrorMessage("Invalid input"))
      case "max" :: t if t.nonEmpty                => Right(Command.Max(t.map(x => x.toDouble)))
      case "max" :: _                              => Left(ErrorMessage("Invalid input"))
      case _                                       => Left(ErrorMessage("Unrecognized command type"))
    }
  }

  def calculate(command: Command): Either[ErrorMessage, Result] = ???

  def calculate(command: Command): String = command match {
    case Command.Divide(dividend, divisor) => (dividend / divisor).toString
    case Command.Sum(numbers)              => (numbers.sum).toString
    case Command.Average(numbers)          => (numbers.sum / numbers.length).toString
    case Command.Min(numbers)              => (numbers.min).toString
    case Command.Max(numbers)              => (numbers.max).toString
  }

  def renderResult(x: Result): String = {
    ??? // implement this method
  }

  def process(line: String): String = {
    parseCommand(line)
    ???
  }

  def main(args: Array[String]): Unit  = Source.stdin.getLines() map process foreach println
}
