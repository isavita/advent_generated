
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val total = Source.fromFile("input.txt")
      .getLines()
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(calculateMaxJoltage)
      .sum
    println(s"Total output joltage: $total")
  }

  def calculateMaxJoltage(bank: String): Int = {
    for (d1 <- 9 to 0 by -1) {
      val ch = ('0' + d1).toChar
      val idx = bank.indexOf(ch)
      if (idx != -1 && idx < bank.length - 1) {
        val maxD2 = bank
          .substring(idx + 1)
          .filter(_.isDigit)
          .map(_ - '0')
          .foldLeft(-1)(Math.max)
        if (maxD2 != -1) return d1 * 10 + maxD2
      }
    }
    0
  }
}
