
import scala.io.Source

object Main {
  def parseInput(input: Array[String]): Array[Array[Int]] = {
    input.map(parseStringToInts)
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
    Iterator.iterate(history)(calculateExtrapolation)
      .takeWhile(arr => !allZeros(arr))
      .toArray
  }

  def solve(input: Array[String]): Int = {
    val histories = parseInput(input)
    histories.map(history => {
      val extrapolationsSeries = calculateExtrapolations(history)
      extrapolationsSeries.foldRight(0)((arr, acc) => arr.last + acc)
    }).sum
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toArray
    println(solve(input))
  }
}
