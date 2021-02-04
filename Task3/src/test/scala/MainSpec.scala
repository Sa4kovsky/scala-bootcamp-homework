
import Main._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class MainSpec extends AnyFlatSpec {

  "divide" should "give divide 4 5 output '4.0 divided by 5.0 is 0.8' " in {
    process("divide 4 5") shouldEqual "4.0 divided by 5.0 is 0.8"
  }

  "sum" should "give sum 5 5 6 8.5 output 'the sum of 5 5 6 8.5 is 24.5' " in {
    process("sum 5 5 6 8.5") shouldEqual "the sum of 5.0 5.0 6.0 8.5 is 24.5"
  }

  "average" should "give average 4 3 8.5 4 output 'the average of 4 3 8.5 4 is 4.875' " in {
    process("average 4 3 8.5 4") shouldEqual "the average of 4.0 3.0 8.5 4.0 is 4.875"
  }

  "min" should "give min 4 -3 -17 output 'the minimum of 4 -3 -17 is -17' " in {
    process("min 4 -3 -17") shouldEqual "the minimum of 4.0 -3.0 -17.0 is -17.0"
  }

  "min" should "give max 4 -3 -17 output 'the maximum of 4 -3 -17 is 4' " in {
    process("max 4 -3 -17") shouldEqual "the maximum of 4.0 -3.0 -17.0 is 4.0"
  }

  "error" should "give divide 4 5 5 output 'Error: Invalid input' " in {
    process("divide 4 5 5") shouldEqual "Error: Invalid input"
  }

  "error" should "give divide 4 5,2 5 output 'Error: Invalid input' " in {
    process("divide 4 5,2 5") shouldEqual "Error: Invalid input"
  }

  "error" should "give divide 4r 5  output 'Error: Invalid input' " in {
    process("divide 4r 5") shouldEqual "Error: Invalid input"
  }

  "error" should "give divide output 'Error: Invalid input' " in {
    process("divide") shouldEqual "Error: Invalid input"
  }

  "error" should "give 21312 output 'Error: Unrecognized command type' " in {
    process("21312") shouldEqual "Error: Unrecognized command type"
  }
}
