
import Calculator._

import scala.util.Try

/** Simple calculator with buttons.
 *
 * @param memory whatever is stored in the memory.
 * @param screen whatever you see on the screen.
 */
case class Calculator(memory: Double = 0, screen: Double = 0, operation: Option[Operation] = None) {

  def plus: Calculator = this.copy(operation = Some(Operation.Plus))
  def minus: Calculator = this.copy(operation = Some(Operation.Minus))
  def divide: Calculator = this.copy(operation = Some(Operation.Divide))
  def times: Calculator = this.copy(operation = Some(Operation.Times))

  def calculate(calculator: Calculator):  Either[String, Double] = calculator.operation match{
    case Some(Operation.Plus)   => Right(calculator.memory + calculator.screen)
    case Some(Operation.Minus)  => Right(calculator.memory - calculator.screen)
    case Some(Operation.Divide) => if(calculator.screen != 0) Right(calculator.memory / calculator.screen) else Left(ErrorMessage.ErrorZero.message)
    case Some(Operation.Times)  => Right(calculator.memory * calculator.screen)
    case _                      => Left(ErrorMessage.CalculationError.message)
  }

}

object Calculator {
  sealed trait Operation
  object Operation {
    object Plus extends Operation
    object Minus extends Operation
    object Divide extends Operation
    object Times extends Operation
  }
}

sealed abstract class ErrorMessage(details: String) { def message: String = s"Error: $details" }
object ErrorMessage {
  case object ErrorZero extends ErrorMessage("You can't divide by 0")
  case object CalculationError extends ErrorMessage("Invalid data")
}
