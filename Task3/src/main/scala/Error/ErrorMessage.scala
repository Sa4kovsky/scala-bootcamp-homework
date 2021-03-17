package Error

sealed abstract class ErrorMessage(details: String) { def message: String = s"Error: $details" }
object ErrorMessage {
  case object UnrecognizedCommand extends ErrorMessage("Unrecognized command type")
  case object NotCorrectNumberFormat extends ErrorMessage("Not the correct format")
  case object NotCorrectInputNumber extends ErrorMessage("Not the correct input number")
  case object CalculationError extends ErrorMessage("Calculation Error")
}


