
import scala.io.Source

object BridgeRepair {
  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val lines = Source.fromFile(filename).getLines().toList

    val result = lines.map(processEquation).sum
    println(result)
  }

  def processEquation(line: String): Long = {
    val Array(testValueStr, equationStr) = line.split(": ")
    val testValue = testValueStr.toLong
    val numbers = equationStr.split(" ").map(_.toLong)

    if (canBeTrue(testValue, numbers)) testValue else 0
  }

  def canBeTrue(testValue: Long, numbers: Array[Long]): Boolean = {
    def helper(index: Int, currentValue: Long): Boolean = {
      if (index == numbers.length) currentValue == testValue
      else {
        val nextValue = numbers(index)
        helper(index + 1, currentValue + nextValue) ||
        helper(index + 1, currentValue * nextValue) ||
        helper(index + 1, concatenate(currentValue, nextValue))
      }
    }

    helper(1, numbers(0))
  }

  def concatenate(a: Long, b: Long): Long = {
    (a.toString + b.toString).toLong
  }
}
