import org.scalatest.funsuite.AnyFunSuite

class CalculatorSpec extends AnyFunSuite {


  test("print an error if the input is not correct") {
    val testCalc = Calculator()
    assert(Left("Error: Input is not correct") ==  testCalc.calculate(Calculator()))
  }

  test("test - addition of two positive numbers") {
    val testCalc = Calculator()
    assert(Right(12.0) ==  testCalc.calculate(Calculator(10, 2).plus))
  }

  test("test - addition of a positive and negative number") {
    val testCalc = Calculator()
    assert(Right(-6.0) ==  testCalc.calculate(Calculator(-8, 2).plus))
  }

  test("test - addition of two negative numbers") {
    val testCalc = Calculator()
    assert(Right(-10.0) ==  testCalc.calculate(Calculator(-8, -2).plus))
  }

  test("test - addition of a number and its inverse gives zero") {
    val testCalc = Calculator()
    assert(Right(0.0) ==  testCalc.calculate(Calculator(-1, 1).plus))
  }

  test("test - difference of two positive numbers") {
    val testCalc = Calculator()
    assert(Right(8.0) ==  testCalc.calculate(Calculator(10, 2).minus))
  }

  test("test - difference of positive and negative numbers") {
    val testCalc = Calculator()
    assert(Right(-4.0) ==  testCalc.calculate(Calculator(-2, 2).minus))
  }

  test("test - difference of two negative numbers") {
    val testCalc = Calculator()
    assert(Right(0.0) ==  testCalc.calculate(Calculator(-2, -2).minus))
  }

  test("test - division of two positive gives a positive") {
    val testCalc = Calculator()
    assert(Right(5.0) ==  testCalc.calculate(Calculator(10, 2).divide))
  }

  test("test - division of two negative numbers") {
    val testCalc = Calculator()
    assert(Right(2.0) ==  testCalc.calculate(Calculator(-4, -2).divide))
  }

  test("test - dividing a negative number by its own gives one") {
    val testCalc = Calculator()
    assert(Right(1.0) ==  testCalc.calculate(Calculator(-4, -4).divide))
  }

  test("test - division of positive and negative gives a negative") {
    val testCalc = Calculator()
    assert(Right(-5.0) ==  testCalc.calculate(Calculator(10, -2).divide))
  }

  test("test - dividing zero by a number") {
    val testCalc = Calculator()
    assert(Right(0.0) ==  testCalc.calculate(Calculator(0, 2).divide))
  }

  test("test - dividing a number by zero gives an error") {
    val testCalc = Calculator()
    assert(Left("Error: You can't divide by 0") ==  testCalc.calculate(Calculator(2, 0).divide))
  }

  test("test - multiplying a number by 0 gives 0") {
    val testCalc = Calculator()
    assert(Right(0.0) ==  testCalc.calculate(Calculator(2, 0).times))
  }

  test("test - multiplying of two positive numbers") {
    val testCalc = Calculator()
    assert(Right(20.0) ==  testCalc.calculate(Calculator(2, 10).times))
  }

  test("test - multiplying positive and negative numbers gives a negative") {
    val testCalc = Calculator()
    assert(Right(-20.0) ==  testCalc.calculate(Calculator(2, -10).times))
  }

  test("test - multiplying two negative numbers gives a positive") {
    val testCalc = Calculator()
    assert(Right(20.0) ==  testCalc.calculate(Calculator(-2, -10).times))
  }

}
