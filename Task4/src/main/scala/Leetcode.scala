import java.util

object Leetcode {

  // https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.scan(0)(_ + _).tail
  }

  def runningSum1(nums: Array[Int]): Array[Int] = {
    def calcSum(nums: Array[Int], acc: Array[Int] = Array(0)): Array[Int] = nums match {
      case Array(h, _*) => calcSum(nums.tail, acc :+ (h + acc.last))
      case _            => acc.tail
    }
    calcSum(nums)
  }

  // https://leetcode.com/problems/shuffle-the-array
  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    val array = nums.splitAt(n)
    array._1.zip(array._2).flatMap{ case(a,b) => Array(a,b) }
  }

  def shuffle1(nums: Array[Int], n: Int): Array[Int] = {
    val array = nums.splitAt(n)
    def merge(array1: Array[Int], array2: Array[Int], acc: Array[Int]): Array[Int] = (array1, array2) match {
      case (Array(h1, _*), Array(h2, _*)) => merge(array1.tail, array2.tail, acc :+ h1 :+ h2)
      case (_, _) => acc.tail
    }
    merge(array._1, array._2, Array(0))
  }

  // https://leetcode.com/problems/richest-customer-wealth
  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    val sum = for{
      acc <- accounts
    } yield acc.sum
    sum.max
  }

  def maximumWealth1(accounts: Array[Array[Int]]): Int = {
    accounts.map(_.sum).max
  }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithCandies1(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    candies.map(_ + extraCandies >= candies.max)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val x = points.map(_(0)).sorted
    x.zip(x.tail).map(x => x._2 - x._1).max
  }

  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  def maxDepth(s: String): Int = {
    s.foldLeft(0, 0) {
      case ((c, v), '(') => (c.max(v + 1), v + 1)
      case ((c, v), ')') => (c, v - 1)
      case (z     ,  _ ) => z
    }._1
  }

  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  def balancedStringSplit(s: String): Int = {
    val (r, l, count) = s.indices.foldLeft(0, 0, 0) {
      case ((r, l, count), idx) => {
        val acc = if (s.charAt(idx) == 'R') (r + 1, l, count) else (r, l + 1, count)
        if (acc._1 > 0 && acc._1 == acc._2) (0, 0, count + 1) else acc
      }
    }
    count
  }
}
