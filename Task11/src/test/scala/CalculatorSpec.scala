import org.scalatest.funsuite.AnyFunSuite

class CalculatorSpec extends AnyFunSuite {

  test("1 + 0 = 1") {
    val testCalc = Calculator()
    assert(Right(1.0) ==  testCalc.calculate(Calculator(1, 0).plus))
  }

  test("-8 + 2 = -6") {
    val testCalc = Calculator()
    assert(Right(-6.0) ==  testCalc.calculate(Calculator(-8, 2).plus))
  }

  test("10 + 2 = 12") {
    val testCalc = Calculator()
    assert(Right(12.0) ==  testCalc.calculate(Calculator(10, 2).plus))
  }

  test("10 - 2 = 8") {
    val testCalc = Calculator()
    assert(Right(8.0) ==  testCalc.calculate(Calculator(10, 2).minus))
  }

  test("-2 - 2 = -4") {
    val testCalc = Calculator()
    assert(Right(-4.0) ==  testCalc.calculate(Calculator(-2, 2).minus))
  }

  test("-2 - (-2) = 0") {
    val testCalc = Calculator()
    assert(Right(0.0) ==  testCalc.calculate(Calculator(-2, -2).minus))
  }

  test("10 / 2 = 5") {
    val testCalc = Calculator()
    assert(Right(5.0) ==  testCalc.calculate(Calculator(10, 2).divide))
  }

  test("10 / (-2) = -5") {
    val testCalc = Calculator()
    assert(Right(-5.0) ==  testCalc.calculate(Calculator(10, -2).divide))
  }

  test("0 / 2 = 0") {
    val testCalc = Calculator()
    assert(Right(0.0) ==  testCalc.calculate(Calculator(0, 2).divide))
  }

  test("2 / 0 = error") {
    val testCalc = Calculator()
    assert(Left("Error: You can't divide by 0") ==  testCalc.calculate(Calculator(2, 0).divide))
  }

  test("2 * 0 = 0") {
    val testCalc = Calculator()
    assert(Right(0.0) ==  testCalc.calculate(Calculator(2, 0).times))
  }

  test("2 * 10 = 20") {
    val testCalc = Calculator()
    assert(Right(20.0) ==  testCalc.calculate(Calculator(2, 10).times))
  }

}
