
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().mkString("\n")

    val (nums, boards) = parseInput(input)
    val result = solve(nums, boards)
    println(result)
  }

  def solve(nums: List[Int], boards: List[BoardState]): Int = {
    var lastWinningScore = -1
    var alreadyWon = Map[Int, Boolean]()

    nums.foreach { n =>
      boards.zipWithIndex.foreach { case (b, bi) =>
        if (!alreadyWon.getOrElse(bi, false)) {
          val didWin = b.pickNum(n)
          if (didWin) {
            lastWinningScore = b.score() * n
            alreadyWon += (bi -> true)
          }
        }
      }
    }

    lastWinningScore
  }

  case class BoardState(board: Array[Array[Int]], picked: Array[Array[Boolean]])

  object BoardState {
    def apply(board: Array[Array[Int]]): BoardState = {
      val picked = Array.fill(board.length, board(0).length)(false)
      BoardState(board, picked)
    }
  }

  implicit class BoardStateOps(b: BoardState) {
    def pickNum(num: Int): Boolean = {
      for {
        r <- b.board.indices
        c <- b.board(0).indices
        if b.board(r)(c) == num
      } b.picked(r)(c) = true

      b.board.indices.foreach { i =>
        var isFullRow = true
        var isFullCol = true

        b.board.indices.foreach { j =>
          if (!b.picked(i)(j)) isFullRow = false
          if (!b.picked(j)(i)) isFullCol = false
        }

        if (isFullRow || isFullCol) return true
      }

      false
    }

    def score(): Int = {
      var score = 0

      for {
        r <- b.board.indices
        c <- b.board(0).indices
        if !b.picked(r)(c)
      } score += b.board(r)(c)

      score
    }
  }

  def parseInput(input: String): (List[Int], List[BoardState]) = {
    val lines = input.split("\n\n")

    val nums = lines(0).split(",").map(toInt).toList

    val boards = lines.drop(1).map { grid =>
      val b = grid.split("\n").map { line =>
        line.replaceAll("  ", " ").stripPrefix(" ").split(" ").map(toInt).toArray
      }.toArray

      BoardState(b)
    }.toList

    (nums, boards)
  }

  def toInt(s: String): Int = s.toInt
}
