object Main {

  def gcd(a: Int, b: Int): Int = (a, b) match {
    case (a, 0) => a
    case (a, b) => gcd(b, a % b)
  }

  def lcm(a: Int, b: Int): Option[Int] = (a,b) match {
    case (0, 0) => None
    case _     => Some(math.abs(a * b) / gcd(a, b))
  }
}