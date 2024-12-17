
import java.io.File
import scala.io.Source
import scala.collection.mutable

object Solution {
  def trimLeadingZeros(s: String): String = {
    val i = s.indexWhere(_ != '0')
    if (i == -1 || i == s.length) "0" else s.substring(i)
  }

  def splitStone(s: String): (String, String) = {
    val mid = s.length / 2
    val left = trimLeadingZeros(s.substring(0, mid))
    val right = trimLeadingZeros(s.substring(mid))
    (if (left.isEmpty) "0" else left, if (right.isEmpty) "0" else right)
  }

  def multiplyBy2024(s: String): String = {
    val num = s.map(_ - '0').toArray
    val multiplier = Array(2, 0, 2, 4)
    val result = Array.fill(num.length + multiplier.length)(0)

    for (i <- num.indices.reverse) {
      var carry = 0
      for (j <- multiplier.indices.reverse) {
        val product = num(i) * multiplier(j) + result(i + j + 1) + carry
        result(i + j + 1) = product % 10
        carry = product / 10
      }
      result(i) += carry
    }

    val start = result.indexWhere(_ != 0)
    if (start == -1) "0" else result.slice(start, result.length).mkString
  }

  def main(args: Array[String]): Unit = {
    val file = new File("input.txt")
    val lines = Source.fromFile(file).getLines().toList
    if (lines.isEmpty) {
      println("Input file is empty")
      return
    }
    val stonesStr = lines.head.split("\\s+")

    var stonesMap = mutable.Map[String, Long]().withDefaultValue(0L)
    stonesStr.foreach(s => stonesMap(s) += 1)

    val steps = 75
    for (_ <- 0 until steps) {
      val newStonesMap = mutable.Map[String, Long]().withDefaultValue(0L)
      stonesMap.foreach { case (stone, count) =>
        if (stone == "0") {
          newStonesMap("1") += count
        } else if (stone.length % 2 == 0) {
          val (left, right) = splitStone(stone)
          newStonesMap(left) += count
          newStonesMap(right) += count
        } else {
          val newStone = multiplyBy2024(stone)
          newStonesMap(newStone) += count
        }
      }
      stonesMap = newStonesMap
    }

    val totalStones = stonesMap.values.sum
    println(totalStones)
  }
}
