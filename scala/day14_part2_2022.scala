import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString
    println(solve(input))
  }

  def solve(input: String): Int = {
    val matrix = parseInput(input)
    var originCol = 0
    for (i <- matrix(0).indices) {
      if (matrix(0)(i) == "+") {
        originCol = i
      }
      matrix(matrix.length - 1)(i) = "#"
    }

    var ans = 0
    while (!dropSand(matrix, originCol)) {
      ans += 1
      if (matrix(0)(originCol) == "o") {
        return ans
      }
    }
    ans
  }

  def parseInput(input: String): ArrayBuffer[ArrayBuffer[String]] = {
    val coordSets = input.split("\n").map { line =>
      line.split(" -> ").map { coord =>
        val Array(x, y) = coord.split(",").map(_.toInt)
        (x, y)
      }
    }

    val lowestCol = coordSets.flatMap(_.map(_._1)).min
    val highestRow = coordSets.flatMap(_.map(_._2)).max
    val highestCol = coordSets.flatMap(_.map(_._1)).max
    val extraLeftSpace = 200

    for (set <- coordSets) {
      for (i <- set.indices) {
        set(i) = (set(i)._1 - lowestCol + extraLeftSpace, set(i)._2)
      }
    }

    val matrix = ArrayBuffer.fill(highestRow + 3, highestCol + extraLeftSpace * 2)(".")
    for (set <- coordSets) {
      for (i <- 1 until set.length) {
        val cols = Array(set(i - 1)._1, set(i)._1)
        val rows = Array(set(i - 1)._2, set(i)._2)
        scala.util.Sorting.quickSort(cols)
        scala.util.Sorting.quickSort(rows)

        if (cols(0) == cols(1)) {
          for (r <- rows(0) to rows(1)) {
            matrix(r)(cols(0)) = "#"
          }
        } else if (rows(0) == rows(1)) {
          for (c <- cols(0) to cols(1)) {
            matrix(rows(0))(c) = "#"
          }
        }
      }
    }

    val originCol = 500 - lowestCol + extraLeftSpace
    matrix(0)(originCol) = "+"

    for (i <- matrix.indices; j <- matrix(i).indices) {
      if (matrix(i)(j) == ".") {
        matrix(i)(j) = "."
      }
    }

    matrix
  }

  def dropSand(matrix: ArrayBuffer[ArrayBuffer[String]], originCol: Int): Boolean = {
    var r, c = 0
    r = 0
    c = originCol

    while (r < matrix.length - 1) {
      val below = matrix(r + 1)(c)
      val diagonallyLeft = matrix(r + 1)(c - 1)
      val diagonallyRight = matrix(r + 1)(c + 1)
      if (below == ".") {
        r += 1
      } else if (diagonallyLeft == ".") {
        r += 1
        c -= 1
      } else if (diagonallyRight == ".") {
        r += 1
        c += 1
      } else {
        matrix(r)(c) = "o"
        return false
      }
    }

    true
  }
}