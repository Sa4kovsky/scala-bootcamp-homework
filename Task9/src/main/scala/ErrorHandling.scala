import ErrorHandling.ValidationError._

import cats.data.ValidatedNec
import cats.syntax.all._

object ErrorHandling {

  // Homework. Place the solution under `error_handling` package in your homework repository.
  //
  // 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
  // 2. Add `ValidationError` cases (at least 5, may be more).
  // 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.

  case class Name(firstName: String, lastName: String)

  case class NumberCard(first: String, second: String, third: String, fourth: String)

  case class ExpirationDate(month: String, year: String)

  case class PaymentCard(name: Name,
                         number: NumberCard,
                         expirationDate: ExpirationDate,
                         securityCode: String)

  sealed trait ValidationError

  object ValidationError {

    case object NameFormatError extends ValidationError

    case object NumberCountFormatError extends ValidationError

    case object NumberFormatError extends ValidationError

    case object ExpirationDateFormatError extends ValidationError

    case object SecurityCodeFormatError extends ValidationError

  }

  object PaymentCardValidator {
    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate(
                  name: Name,
                  number: NumberCard,
                  expirationDate: ExpirationDate,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] =
      (
        validateName(name),
        validateNumberCard(number),
        validateExpirationDate(expirationDate),
        validateSecurityCode(securityCode)
      ).mapN((na, nu, ed, sc) => new PaymentCard(name = na, number = nu, expirationDate = ed, securityCode = sc) {})

    def validateName(name: Name): AllErrorsOr[Name] = {
      val nameRegex = "^[a-zA-Z]+$"
      if (name.lastName.matches(nameRegex) && name.firstName.matches(nameRegex)) {
        Name(name.firstName, name.lastName).validNec
      } else NameFormatError.invalidNec
    }

    def validateNumberCard(number: NumberCard): AllErrorsOr[NumberCard] = {
        def validateCountNumberCard(): AllErrorsOr[NumberCard] = {
          val fourDigit = 4
          if (number.first.length == fourDigit && number.second.length == fourDigit && number.third.length == fourDigit && number.fourth.length == fourDigit) {
            NumberCard(number.first, number.second, number.third, number.fourth).validNec
          } else NumberCountFormatError.invalidNec
        }

        def validateCard(): AllErrorsOr[NumberCard] = {
          if (number.first.toIntOption.nonEmpty && number.second.toIntOption.nonEmpty && number.third.toIntOption.nonEmpty && number.fourth.toIntOption.nonEmpty) {
            NumberCard(number.first, number.second, number.third, number.fourth).validNec
          } else NumberFormatError.invalidNec
        }

      validateCountNumberCard().andThen(_ => validateCard())
      }


    def validateExpirationDate(expirationDate: ExpirationDate): AllErrorsOr[ExpirationDate] = {
      if (expirationDate.month.length == 2 && expirationDate.year.length == 4 && expirationDate.month.toIntOption.nonEmpty && expirationDate.year.toIntOption.nonEmpty) {
        ExpirationDate(expirationDate.month, expirationDate.year).validNec
      } else ExpirationDateFormatError.invalidNec
    }

    def validateSecurityCode(securityCode: String):AllErrorsOr[String] = {
      if (securityCode.length == 3 && securityCode.toIntOption.nonEmpty) {
        securityCode.validNec
      } else SecurityCodeFormatError.invalidNec
    }
  }
}
