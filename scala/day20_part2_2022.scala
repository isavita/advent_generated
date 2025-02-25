
import scala.io.Source

object Solution {
  def mix(nums: Array[(Int, Long)]): Unit = {
    val n = nums.length - 1
    for (i <- nums.indices) {
      val oldpos = nums(i)._1
      var newpos = ((oldpos + nums(i)._2) % n + n) % n
      if (oldpos < newpos) {
        for (j <- nums.indices) {
          if (nums(j)._1 > oldpos && nums(j)._1 <= newpos) {
            nums(j) = (nums(j)._1 - 1, nums(j)._2)
          }
        }
      }
      if (newpos < oldpos) {
        for (j <- nums.indices) {
          if (nums(j)._1 >= newpos && nums(j)._1 < oldpos) {
            nums(j) = (nums(j)._1 + 1, nums(j)._2)
          }
        }
      }
      nums(i) = (newpos.toInt, nums(i)._2)
    }
  }

  def coords(nums: Array[(Int, Long)]): Long = {
    val l = nums.length
    val zeroPos = nums.find(_._2 == 0).get._1
    (1000 to 3000 by 1000).map(i => nums.find(_._1 == (zeroPos + i) % l).get._2).sum
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines.toList
    val nums = lines.map(_.toLong).zipWithIndex.map { case (n, i) => (i, n * 811589153L) }.toArray
    for (_ <- 0 until 10) {
      mix(nums)
    }
    println(coords(nums))
  }
}
