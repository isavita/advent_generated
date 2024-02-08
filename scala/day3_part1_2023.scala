
import scala.io.Source

object Main extends App {
  val matrix = readFileToMatrix("input.txt")
  val sum = sumOfPartNumbers(matrix)
  println(sum)

  def readFileToMatrix(filePath: String): Array[Array[Char]] = {
    val source = Source.fromFile(filePath)
    val matrix = source.getLines.map(_.toCharArray).toArray
    source.close()
    matrix
  }

  def sumOfPartNumbers(matrix: Array[Array[Char]]): Int = {
    var sum = 0
    val visited = Array.ofDim[Boolean](matrix.length, matrix(0).length)

    for (y <- matrix.indices) {
      for (x <- matrix(y).indices) {
        if (!visited(y)(x) && matrix(y)(x).isDigit) {
          val (number, length) = extractNumber(matrix, x, y)
          if (isAdjacentToSymbol(matrix, x, y, length)) {
            sum += number
          }
          for (i <- 0 until length) {
            visited(y)(x + i) = true
          }
        }
      }
    }
    sum
  }

  def extractNumber(matrix: Array[Array[Char]], x: Int, y: Int): (Int, Int) = {
    var numberStr = ""
    var newX = x
    while (newX < matrix(y).length && matrix(y)(newX).isDigit) {
      numberStr += matrix(y)(newX)
      newX += 1
    }
    (numberStr.toInt, numberStr.length)
  }

  def isAdjacentToSymbol(matrix: Array[Array[Char]], x: Int, y: Int, length: Int): Boolean = {
    for (i <- 0 until length) {
      if (checkAdjacent(matrix, x + i, y)) {
        return true
      }
    }
    false
  }

  def checkAdjacent(matrix: Array[Array[Char]], x: Int, y: Int): Boolean = {
    for {
      dy <- -1 to 1
      dx <- -1 to 1
    } {
      val adjX = x + dx
      val adjY = y + dy
      if (adjY >= 0 && adjY < matrix.length && adjX >= 0 && adjX < matrix(adjY).length) {
        if (!matrix(adjY)(adjX).isDigit && matrix(adjY)(adjX) != '.') {
          return true
        }
      }
    }
    false
  }
}
