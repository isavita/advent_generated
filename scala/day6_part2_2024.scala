
import scala.io.Source

object Solution {
  case class State(x: Int, y: Int, dir: Int)

  def main(args: Array[String]): Unit = {
    val grid = Source.fromFile("input.txt").getLines().map(_.toArray).toArray
    val h = grid.length
    val w = grid(0).length

    var (startX, startY, startDir) = (0, 0, 0)
    for (y <- 0 until h; x <- 0 until w) {
      grid(y)(x) match {
        case '^' => startX = x; startY = y; startDir = 0
        case '>' => startX = x; startY = y; startDir = 1
        case 'v' => startX = x; startY = y; startDir = 2
        case '<' => startX = x; startY = y; startDir = 3
        case _ =>
      }
    }
    grid(startY)(startX) = '.'

    var canLoop = 0
    for (y <- 0 until h; x <- 0 until w) {
      if (x == startX && y == startY) {}
      else if (grid(y)(x) == '.') {
        grid(y)(x) = '#'
        if (loops(grid, startX, startY, startDir)) {
          canLoop += 1
        }
        grid(y)(x) = '.'
      }
    }
    println(canLoop)
  }

  def loops(grid: Array[Array[Char]], sx: Int, sy: Int, sdir: Int): Boolean = {
    val h = grid.length
    val w = grid(0).length
    val dirs = Array((0, -1), (1, 0), (0, 1), (-1, 0))
    var (x, y, dir) = (sx, sy, sdir)
    var seen = Set[State]()
    for (_ <- 0 until 2000000) {
      val st = State(x, y, dir)
      if (seen.contains(st)) {
        return true
      }
      seen += st
      val (dx, dy) = dirs(dir)
      val (nx, ny) = (x + dx, y + dy)
      if (nx < 0 || nx >= w || ny < 0 || ny >= h) {
        return false
      }
      if (grid(ny)(nx) == '#') {
        dir = (dir + 1) % 4
      } else {
        x = nx
        y = ny
      }
    }
    false
  }
}
