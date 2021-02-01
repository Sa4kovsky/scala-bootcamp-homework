

def area(
    x1: Double,
    y1: Double,
    x2: Double,
    y2: Double,
    x3: Double,
    y3: Double
): Double = {
  val lengthP1P2: Double =
    Math.sqrt(Math.pow(x1 - x2, 2) + Math.pow(y1 - y2, 2))
  val lengthP2P3: Double =
    Math.sqrt(Math.pow(x2 - x3, 2) + Math.pow(y2 - y3, 2))
  val lengthP1P3: Double =
    Math.sqrt(Math.pow(x1 - x3, 2) + Math.pow(y1 - y3, 2))
  val semiperimeter: Double = (lengthP1P2 + lengthP2P3 + lengthP1P3) / 2

  Math.sqrt(
    semiperimeter * (semiperimeter - lengthP1P2) * (semiperimeter - lengthP2P3) * (semiperimeter - lengthP1P3)
  )
}

area(1, 1, 2, 2, 1, 3)

