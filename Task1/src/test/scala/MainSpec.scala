
import Main._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper


class MainSpec extends AnyFlatSpec  {

  "gcd" should "give 9 for 9 and 0" in {
    gcd(9, 0) shouldEqual 9
  }

  "gcd" should "give 3 for 9 and 12" in {
    gcd(9, 12) shouldEqual 3
  }

  "gcd" should "give 6 for 24 and 18" in {
    gcd(24, 18) shouldEqual 6
  }

  "gcd" should "give 20 for 100 and 40" in {
    gcd(100, 40) shouldEqual 20
  }

  "lcm" should "give 0 for 0 and 12" in {
    lcm(0, 12) shouldEqual Some(0)
  }

  "lcm" should "give 36 for 9 and 12" in {
    lcm(9, 12) shouldEqual Some(36)
  }

  "lcm" should "give 900 for 50 and 180" in {
    lcm(50, 180) shouldEqual Some(900)
  }

  "lcm" should "give 0 for 0 and None" in {
    lcm(0, 0) shouldEqual None
  }
}
