import Command.Command
import Convert._
import Error.ErrorMessage
import Result._

import scala.io.Source

object Main {

  def parseCommand(line: String): Either[ErrorMessage, Command] = {
    line.toLowerCase.split("\\s+").toList match {
      case "divide" :: Nil                        => Left(ErrorMessage.NotCorrectInputNumber)
      case "divide" :: dividend :: divisor :: Nil => Convert.convertDouble(dividend, divisor)
      case "sum" :: Nil                           => Left(ErrorMessage.NotCorrectInputNumber)
      case "sum" :: tail                          => Convert.convertListDouble("sum",tail)
      case "average" :: Nil                       => Left(ErrorMessage.NotCorrectInputNumber)
      case "average" :: tail                      => Convert.convertListDouble("average",tail)
      case "min" :: Nil                           => Left(ErrorMessage.NotCorrectInputNumber)
      case "min" :: tail                          => Convert.convertListDouble("min",tail)
      case "max" :: Nil                           => Left(ErrorMessage.NotCorrectInputNumber)
      case "max" :: tail                          => Convert.convertListDouble("max",tail)
      case _                                      => Left(ErrorMessage.UnrecognizedCommand)
    }
  }

  def calculate(command: Command): Either[ErrorMessage, Result] = command match {
    case Command.Divide(dividend, 0)       => Left(ErrorMessage.NotCorrectInputNumber)
    case Command.Divide(dividend, divisor) => Right(ChangeMe(Command.Divide(dividend, divisor), dividend / divisor))
    case Command.Sum(numbers)              => Right(ChangeMe(Command.Sum(numbers), numbers.sum))
    case Command.Average(numbers)          => Right(ChangeMe(Command.Average(numbers), numbers.sum / numbers.length))
    case Command.Min(numbers)              => Right(ChangeMe(Command.Min(numbers), numbers.min))
    case Command.Max(numbers)              => Right(ChangeMe(Command.Max(numbers), numbers.max))
  }

  def renderResult(result: Result): String = result match {
    case ChangeMe(Command.Divide(dividend, divisor), result)  => s"${dividend} divided by ${divisor} is $result"
    case ChangeMe(Command.Sum(numbers), result)               => s"the sum of ${numbers.mkString(" ")} is $result"
    case ChangeMe(Command.Average(numbers), result)           => s"the average of ${numbers.mkString(" ")} is $result"
    case ChangeMe(Command.Max(numbers), result)               => s"the maximum of ${numbers.mkString(" ")} is $result"
    case ChangeMe(Command.Min(numbers), result)               => s"the minimum of ${numbers.mkString(" ")} is $result"
  }

  def process(line: String): String = {
    val process = for {
      parse <- parseCommand(line)
      calc <- calculate(parse)
    } yield renderResult(calc)

    process match {
      case Left(error) => error.message
      case Right(value) => value
    }
  }

  def main(args: Array[String]): Unit  = Source.stdin.getLines() map process foreach println
}
