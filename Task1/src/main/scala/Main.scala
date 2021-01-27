object Main {

  def gcd(a: Int, b: Int): Int = (a, b) match {
    case (aa, 0) => aa
    case (aa, bb) => gcd(bb, aa % bb)
  }

  def lcm(a: Int, b: Int): Int = math.abs(a * b) / gcd(a, b)
}
