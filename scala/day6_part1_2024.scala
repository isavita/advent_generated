
import scala.io.Source

object Solution extends App {
  val grid = Source.fromFile("input.txt").getLines.map(_.toCharArray).toArray
  val h = grid.length
  val w = grid(0).length
  val dirs = Array((0, -1), (1, 0), (0, 1), (-1, 0))
  var (x, y, dirX, dirY, dirIdx) = (0, 0, 0, 0, 0)
  var found = false
  for (i <- 0 until h if !found; j <- 0 until w if !found) {
    grid(i)(j) match {
      case '^' => x = j; y = i; dirIdx = 0; dirX = dirs(dirIdx)._1; dirY = dirs(dirIdx)._2; found = true
      case '>' => x = j; y = i; dirIdx = 1; dirX = dirs(dirIdx)._1; dirY = dirs(dirIdx)._2; found = true
      case 'v' => x = j; y = i; dirIdx = 2; dirX = dirs(dirIdx)._1; dirY = dirs(dirIdx)._2; found = true
      case '<' => x = j; y = i; dirIdx = 3; dirX = dirs(dirIdx)._1; dirY = dirs(dirIdx)._2; found = true
      case _ =>
    }
  }
  val visited = scala.collection.mutable.Set((x, y))
  var (nx, ny) = (0, 0)
  while (true) {
    nx = x + dirX
    ny = y + dirY
    if (nx < 0 || nx >= w || ny < 0 || ny >= h) {
      println(visited.size)
      System.exit(0)
    }
    if (grid(ny)(nx) == '#') {
      dirIdx = (dirIdx + 1) % 4
      dirX = dirs(dirIdx)._1
      dirY = dirs(dirIdx)._2
    } else {
      x = nx
      y = ny
      visited += ((x, y))
    }
  }
}
