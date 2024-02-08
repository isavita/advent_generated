
import scala.io.Source

object Main {
  def parseInput(input: Array[String]): Array[Array[Int]] = {
    input.map(line => parseStringToInts(line))
  }

  def parseStringToInts(numbersLine: String): Array[Int] = {
    numbersLine.split(" ").map(_.toInt)
  }

  def allZeros(nums: Array[Int]): Boolean = {
    nums.forall(_ == 0)
  }

  def calculateExtrapolation(history: Array[Int]): Array[Int] = {
    history.sliding(2).map(pair => pair(1) - pair(0)).toArray
  }

  def calculateExtrapolations(history: Array[Int]): Array[Array[Int]] = {
    var extrapolationsSeries = Array(history)

    for (i <- 1 until history.length) {
      val previousExtrapolations = extrapolationsSeries(i - 1)
      if (allZeros(previousExtrapolations)) {
        return extrapolationsSeries
      }

      val extrapolations = calculateExtrapolation(previousExtrapolations)
      extrapolationsSeries :+= extrapolations
    }

    extrapolationsSeries
  }

  def solve(input: Array[String]): Int = {
    val histories = parseInput(input)
    var res = 0

    for (history <- histories) {
      val extrapolationsSeries = calculateExtrapolations(history)

      var pastPrediction = 0
      for (i <- extrapolationsSeries.length - 1 to 0 by -1) {
        pastPrediction = extrapolationsSeries(i)(0) - pastPrediction
      }

      res += pastPrediction
    }

    res
  }

  def readFile(fileName: String): Array[String] = {
    Source.fromFile(fileName).getLines().toArray
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("input.txt")
    println(solve(input))
  }
}
