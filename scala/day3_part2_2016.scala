import scala.io.Source
import scala.util.Try

object Main {
  def main(args: Array[String]): Unit = {
    val numbers = Source.fromFile("input.txt").getLines().map { line =>
      line.split("\\s+").map(string => Try(string.toInt).toOption).flatten
    }.toList

    val validTriangles = (0 until numbers.head.length).map { i =>
      (0 until numbers.length by 3).count { j =>
        isValidTriangle(numbers(j)(i), numbers(j + 1)(i), numbers(j + 2)(i))
      }
    }.sum

    println(validTriangles)
  }

  def isValidTriangle(a: Int, b: Int, c: Int): Boolean = a + b > c && a + c > b && b + c > a
}