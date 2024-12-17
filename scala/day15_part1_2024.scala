
import scala.io.Source
import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toSeq
    val (gridLines, moves) = lines.partition(_.contains("#"))
    val grid = gridLines.map(_.toCharArray).toArray
    val movesStr = moves.mkString

    var robotR = 0
    var robotC = 0
    for (r <- grid.indices; c <- grid(r).indices) {
      if (grid(r)(c) == '@') {
        robotR = r
        robotC = c
      }
    }

    val dirs = Map(
      '^' -> (-1, 0),
      'v' -> (1, 0),
      '<' -> (0, -1),
      '>' -> (0, 1)
    )

    def pushBoxes(r: Int, c: Int, dr: Int, dc: Int): Boolean = {
      val nr = r + dr
      val nc = c + dc
      if (grid(nr)(nc) == '#') {
        return false
      }
      if (grid(nr)(nc) == 'O') {
        if (!pushBoxes(nr, nc, dr, dc)) {
          return false
        }
      }
      if (grid(nr)(nc) == '.') {
        grid(nr)(nc) = 'O'
        grid(r)(c) = '.'
        return true
      }
      false
    }

    for (move <- movesStr) {
      val (dr, dc) = dirs(move)
      val nr = robotR + dr
      val nc = robotC + dc
      if (grid(nr)(nc) == '#') {
        
      } else if (grid(nr)(nc) == 'O') {
        if (!pushBoxes(nr, nc, dr, dc)) {
          
        } else {
          grid(robotR)(robotC) = '.'
          grid(nr)(nc) = '@'
          robotR = nr
          robotC = nc
        }
      } else if (grid(nr)(nc) == '.' ) {
        grid(robotR)(robotC) = '.'
        grid(nr)(nc) = '@'
        robotR = nr
        robotC = nc
      }
    }

    var sum = 0
    for (r <- grid.indices; c <- grid(r).indices) {
      if (grid(r)(c) == 'O') {
        sum += r * 100 + c
      }
    }

    println(sum)
  }
}
