import scala.annotation.tailrec

def applyNTimesForInts(f: Int => Int, n: Int): Int => Int = { x: Int =>
  n match {
    case 0 => x
    case _ => applyNTimesForInts(f, n - 1)(f(x))
  }
}

def applyNTimesForIntsTailrec(f: Int => Int, n: Int): Int => Int = { x: Int =>
  @tailrec
  def sum(n: Int, acc: Int): Int =
    n match {
      case 0 => acc
      case _ => sum(n - 1, f(acc))
    }
  sum(n, x)
}

applyNTimesForInts(_ + 1, 4)(3)
applyNTimesForIntsTailrec(_ + 1, 4)(3)

def applyNTimes[A](f: A => A, n: Int): A => A = { x: A =>
  n match {
    case 0 => x
    case _ => applyNTimesForInts(f, n - 1)(f(x))
  }
  println(n)
  f(x)
}

applyNTimes((_): Int, 4)(3)