import org.scalatest.funsuite.AnyFunSuite

class CalculatorSpec extends AnyFunSuite {

  test("name of the test 1") {
    val calculator = Calculator()
    assert(Right(25) ==  calculator.calculate(Calculator(1, 0).plus))
  }
}
