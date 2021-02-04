package Convert

import Command.Command
import Error.ErrorMessage

import scala.util.Try

object Convert {
  def isDouble(aString: String): Boolean = Try(aString.toDouble).isSuccess
  def convertDouble(dividend:String, divisor:String): Either[ErrorMessage, Command] = {
    if(isDouble(dividend) && isDouble(divisor)){
      Right(Command.Divide(dividend.toDouble, divisor.toDouble))
    }
    else{
      Left(ErrorMessage("Error: Invalid input"))
    }
  }

  def convertListDouble(command: String, numbers: List[String]): Either[ErrorMessage, Command] = {
    if(numbers.map(x => isDouble(x)).foldLeft(true)(_ && _)){
      command match {
        case "sum" => Right(Command.Sum(numbers.map(x => x.toDouble)))
        case "average" => Right(Command.Average(numbers.map(x => x.toDouble)))
        case "min" => Right(Command.Min(numbers.map(x => x.toDouble)))
        case "max" => Right(Command.Max(numbers.map(x => x.toDouble)))
      }
    }
    else{
      Left(ErrorMessage("Error: Invalid input"))
    }
  }
}
