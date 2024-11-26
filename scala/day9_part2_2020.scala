
import scala.io.Source

object EncodingError {
  def findFirstInvalidNumber(numbers: List[Long], preambleLength: Int): Long = {
    numbers.drop(preambleLength).zipWithIndex.find { case (num, index) =>
      val preamble = numbers.slice(index, index + preambleLength)
      !hasTwoSumPair(num, preamble)
    }.map(_._1).getOrElse(-1)
  }

  def hasTwoSumPair(target: Long, preamble: List[Long]): Boolean = {
    preamble.combinations(2).exists(_.sum == target)
  }

  def findContiguousRange(numbers: List[Long], target: Long): Long = {
    (2 to numbers.length).flatMap { rangeSize =>
      numbers.sliding(rangeSize).find(_.sum == target)
    }.map(range => range.min + range.max)
      .head
  }

  def main(args: Array[String]): Unit = {
    val numbers = Source.fromFile("input.txt").getLines().map(_.toLong).toList

    // Part 1
    val invalidNumber = findFirstInvalidNumber(numbers, 25)
    println(s"First invalid number: $invalidNumber")

    // Part 2
    val encryptionWeakness = findContiguousRange(numbers, invalidNumber)
    println(s"Encryption weakness: $encryptionWeakness")
  }
}
