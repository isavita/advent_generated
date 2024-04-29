import scala.io.Source
import scala.util.Try

case class Board(numbers: Array[Array[Int]]) {
  var marked: Array[Array[Boolean]] = Array.fill(5, 5)(false)

  def markNumber(n: Int): Boolean = {
    for {
      i <- numbers.indices
      j <- numbers(i).indices
    } {
      if (numbers(i)(j) == n) {
        marked(i)(j) = true
        return checkWin()
      }
    }
    false
  }

  def checkWin(): Boolean = {
    for (i <- 0 until 5) {
      if ((0 until 5).forall(j => marked(i)(j)) || (0 until 5).forall(j => marked(j)(i))) {
        return true
      }
    }
    false
  }

  def unmarkedSum(): Int = {
    var sum = 0
    for {
      i <- numbers.indices
      j <- numbers(i).indices
    } {
      if (!marked(i)(j)) {
        sum += numbers(i)(j)
      }
    }
    sum
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    val numbers = input.head.split(",").map(_.toInt)
    val boards = input.tail.filter(_.nonEmpty).grouped(5).map { group =>
      group.map(_.trim.split("\\s+").map(_.toInt)).toArray
    }.map(Board(_)).toSeq

    var winningBoard: Option[Board] = None
    var winningNumber: Int = 0
    for (number <- numbers) {
      for (board <- boards) {
        if (board.markNumber(number)) {
          winningBoard = Some(board)
          winningNumber = number
        }
      }
      if (winningBoard.isDefined) {
        val unmarkedSum = winningBoard.get.unmarkedSum()
        println(s"Final score: ${unmarkedSum * winningNumber}")
        return
      }
    }
  }
}