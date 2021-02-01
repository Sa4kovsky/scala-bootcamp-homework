object ClassesAndTraits {

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Shape extends Located with Bounded {
    def area: Option[Double]
  }

  final case class Point(x: Double, y: Double) extends Shape {
    override val minX: Double = x
    override val maxX: Double = x
    override val minY: Double = y
    override val maxY: Double = y

    override def area: Option[Double] = None
  }

  sealed case class Circle(centerX: Double, centerY: Double, radius: Double)
      extends Shape {
    override val x: Double = centerX
    override val y: Double = centerY
    override val minX: Double = centerX - radius
    override val maxX: Double = centerX + radius
    override val minY: Double = centerY - radius
    override val maxY: Double = centerY + radius

    override val area: Option[Double] = Some(Math.PI * Math.pow(radius, 2))
  }

  sealed case class Rectangle(point: Point, height: Double, width: Double)
      extends Shape {
    override def x: Double = point.x
    override def y: Double = point.y

    override val minX: Double = point.x
    override val maxX: Double = point.x + width
    override val minY: Double = point.y
    override val maxY: Double = point.y + height

    override val area: Option[Double] = Some(height * width)
  }

  sealed case class Triangle(p1: Point, p2: Point, p3: Point) extends Shape {
    val list = List(p1, p2, p3)

    override def x: Double = list.map(_.x).min
    override def y: Double = list.map(_.y).min

    override val minX: Double = list.map(_.x).min
    override val maxX: Double = list.map(_.x).max
    override val minY: Double = list.map(_.y).min
    override val maxY: Double = list.map(_.y).max

    def area: Option[Double] = {
      val lengthP1P2: Double =
        Math.sqrt(Math.pow(p1.x - p2.x, 2) + Math.pow(p1.y - p2.y, 2))
      val lengthP2P3: Double =
        Math.sqrt(Math.pow(p2.x - p3.x, 2) + Math.pow(p2.y - p3.y, 2))
      val lengthP1P3: Double =
        Math.sqrt(Math.pow(p1.x - p3.x, 2) + Math.pow(p1.y - p3.y, 2))
      val semiperimeter: Double = (lengthP1P2 + lengthP2P3 + lengthP1P3) / 2

      Some(
        Math.sqrt(
          semiperimeter * (semiperimeter - lengthP1P2) * (semiperimeter - lengthP2P3) * (semiperimeter - lengthP1P3)
        )
      )
    }

  }

  //3D
  sealed trait Located3D extends Located {
    def z: Double
  }

  sealed trait Bounded3D extends Bounded {
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Shape3D extends Located3D with Bounded3D {
    def volume: Option[Double]
    def square: Option[Double]
  }

  sealed case class Point3D(point: Point, z: Double) extends Shape3D {
    override def x: Double = point.x
    override def y: Double = point.y

    override val minX: Double = x
    override val maxX: Double = x
    override val minY: Double = y
    override val maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z

    override def volume: Option[Double] = None
    override def square: Option[Double] = None
  }

  sealed case class Sphere(point: Point3D, radius: Double) extends Shape3D {
    override def x: Double = point.x
    override def y: Double = point.y
    override def z: Double = point.z

    override def minX: Double = point.x - radius
    override def maxX: Double = point.x + radius
    override def minY: Double = point.y - radius
    override def maxY: Double = point.y + radius
    override def minZ: Double = point.z - radius
    override def maxZ: Double = point.z + radius

    override def volume: Option[Double] =
      Some(4 / 3 * Math.PI * Math.pow(radius, 3))
    override def square: Option[Double] =
      Some(4 * Math.PI * Math.pow(radius, 2))
  }

  sealed case class Cube(point: Point3D, sideLength: Double) extends Shape3D {
    override def x: Double = point.x
    override def y: Double = point.y
    override def z: Double = point.z

    override def minX: Double = x
    override def maxX: Double = x + sideLength
    override def minY: Double = y
    override def maxY: Double = y + sideLength
    override def minZ: Double = z
    override def maxZ: Double = z + sideLength

    override def volume: Option[Double] = Some(Math.pow(sideLength, 3))
    override def square: Option[Double] = Some(6 * Math.pow(sideLength, 2))
  }

  sealed case class Cuboid(
      point: Point3D,
      width: Double,
      height: Double,
      length: Double
  ) extends Shape3D {
    override def x: Double = point.x
    override def y: Double = point.y
    override def z: Double = point.z

    override def minX: Double = x
    override def maxX: Double = x + width
    override def minY: Double = y
    override def maxY: Double = y + height
    override def minZ: Double = z
    override def maxZ: Double = z + length

    override def volume: Option[Double] = ???
    override def square: Option[Double] = ???
  }

  sealed case class Triangle3D(n1: Point3D, n2: Point3D, n3: Point3D)
      extends Shape3D {
    private val list = List(n1, n2, n3)

    override def x: Double = list.map(_.x).min
    override def y: Double = list.map(_.y).min
    override def z: Double = list.map(_.z).min

    override def minX: Double = list.map(_.x).min
    override def maxX: Double = list.map(_.x).max
    override def minY: Double = list.map(_.y).min
    override def maxY: Double = list.map(_.y).max
    override def minZ: Double = list.map(_.z).min
    override def maxZ: Double = list.map(_.z).max

    override def volume: Option[Double] = ???
    override def square: Option[Double] = ???
  }
}
