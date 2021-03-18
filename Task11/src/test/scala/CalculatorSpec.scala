import org.scalatest.funsuite.AnyFunSuite

class CalculatorSpec extends AnyFunSuite {


  test("print an error if the input is not correct") {
    val testCalc = Calculator()
    assert(Left("Error: Input is not correct") ==  testCalc.calculate(Calculator()))
  }

  test("work correctly when the addition 1 + 0 is 1") {
    val testCalc = Calculator()
    assert(Right(1.0) ==  testCalc.calculate(Calculator(1, 0).plus))
  }

  test("work correctly when the addition -8 + 2 is -6") {
    val testCalc = Calculator()
    assert(Right(-6.0) ==  testCalc.calculate(Calculator(-8, 2).plus))
  }

  test("work correctly when the addition 10 + 2 is 12") {
    val testCalc = Calculator()
    assert(Right(12.0) ==  testCalc.calculate(Calculator(10, 2).plus))
  }

  test("work correctly when subtracting 10 - 2 is 8") {
    val testCalc = Calculator()
    assert(Right(8.0) ==  testCalc.calculate(Calculator(10, 2).minus))
  }

  test("work correctly when subtracting -2 - 2 is -4") {
    val testCalc = Calculator()
    assert(Right(-4.0) ==  testCalc.calculate(Calculator(-2, 2).minus))
  }

  test("work correctly when subtracting -2 - (-2) is 0") {
    val testCalc = Calculator()
    assert(Right(0.0) ==  testCalc.calculate(Calculator(-2, -2).minus))
  }

  test("works correctly when the division 10 / 2 is 5") {
    val testCalc = Calculator()
    assert(Right(5.0) ==  testCalc.calculate(Calculator(10, 2).divide))
  }

  test("works correctly when the division 10 / (-2) is -5") {
    val testCalc = Calculator()
    assert(Right(-5.0) ==  testCalc.calculate(Calculator(10, -2).divide))
  }

  test("works correctly when the division 0 / 2 is 0") {
    val testCalc = Calculator()
    assert(Right(0.0) ==  testCalc.calculate(Calculator(0, 2).divide))
  }

  test("outputs an error when dividing by 0") {
    val testCalc = Calculator()
    assert(Left("Error: You can't divide by 0") ==  testCalc.calculate(Calculator(2, 0).divide))
  }

  test("work correctly when multiplying 2 * 0 is 0") {
    val testCalc = Calculator()
    assert(Right(0.0) ==  testCalc.calculate(Calculator(2, 0).times))
  }

  test("work correctly when multiplying 2 * 10 is 20") {
    val testCalc = Calculator()
    assert(Right(20.0) ==  testCalc.calculate(Calculator(2, 10).times))
  }

}
