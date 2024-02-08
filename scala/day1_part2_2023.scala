import scala.io.Source

object Solution {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input.txt")
    var sum = 0

    for (line <- file.getLines) {
      val (firstDigit, lastDigit) = findFirstAndLastDigit(line)
      sum += 10 * firstDigit + lastDigit
    }

    println(sum)
    file.close()
  }

  def findFirstAndLastDigit(line: String): (Int, Int) = {
    val digits = Array("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    var firstDigit = 0
    var lastDigit = 0

    for ((char, index) <- line.zipWithIndex) {
      val digitStr = char.toString
      if (digitStr.forall(_.isDigit)) {
        if (firstDigit == 0) {
          firstDigit = digitStr.toInt
        }
        lastDigit = digitStr.toInt
      } else {
        for ((digit, j) <- digits.zipWithIndex) {
          if (line.slice(index, line.length).startsWith(digit)) {
            if (firstDigit == 0) {
              firstDigit = j
            }
            lastDigit = j
          }
        }
      }
    }

    (firstDigit, lastDigit)
  }
}