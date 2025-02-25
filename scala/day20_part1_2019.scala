
import scala.collection.mutable
import scala.io.Source

object MazeSolver {

  def readMaze(filename: String): Array[Array[Char]] =
    Source.fromFile(filename).getLines().map(_.toCharArray).toArray

  def findPortals(maze: Array[Array[Char]]): (Map[String, List[(Int, Int)]], Map[(Int, Int), String]) = {
    val height = maze.length
    val width = maze(0).length
    val portals = mutable.Map[String, List[(Int, Int)]]().withDefaultValue(Nil)
    val portalPositions = mutable.Map[(Int, Int), String]()

    for (y <- 0 until height; x <- 0 until width)
      if (maze(y)(x).isUpper) {
        if (x + 1 < width && maze(y)(x + 1).isUpper) {
          val portalName = s"${maze(y)(x)}${maze(y)(x + 1)}"
          if (x + 2 < width && maze(y)(x + 2) == '.') {
            portals(portalName) ::= (x + 2, y)
            portalPositions((x + 2, y)) = portalName
          } else if (x - 1 >= 0 && maze(y)(x - 1) == '.') {
            portals(portalName) ::= (x - 1, y)
            portalPositions((x - 1, y)) = portalName
          }
        }
        if (y + 1 < height && maze(y + 1)(x).isUpper) {
          val portalName = s"${maze(y)(x)}${maze(y + 1)(x)}"
          if (y + 2 < height && maze(y + 2)(x) == '.') {
            portals(portalName) ::= (x, y + 2)
            portalPositions((x, y + 2)) = portalName
          } else if (y - 1 >= 0 && maze(y - 1)(x) == '.') {
            portals(portalName) ::= (x, y - 1)
            portalPositions((x, y - 1)) = portalName
          }
        }
      }
    (portals.toMap, portalPositions.toMap)
  }

  def bfs(
      maze: Array[Array[Char]],
      portals: Map[String, List[(Int, Int)]],
      portalPositions: Map[(Int, Int), String],
      start: (Int, Int),
      end: (Int, Int)
  ): Int = {
    val queue = mutable.Queue[(Int, Int, Int)]((start._1, start._2, 0))
    val visited = mutable.Set(start)

    while (queue.nonEmpty) {
      val (x, y, steps) = queue.dequeue()

      for ((dx, dy) <- Seq((-1, 0), (1, 0), (0, -1), (0, 1))) {
        val (nx, ny) = (x + dx, y + dy)
        if (0 <= nx && nx < maze(0).length && 0 <= ny && ny < maze.length && maze(ny)(nx) == '.') {
          if ((nx, ny) == end) return steps + 1
          if (!visited((nx, ny))) {
            visited += ((nx, ny))
            queue.enqueue((nx, ny, steps + 1))
          }
        }
      }

      if (portalPositions.contains((x, y))) {
        val portalName = portalPositions((x, y))
        for ((px, py) <- portals(portalName) if (px, py) != (x, y) && !visited((px, py))) {
          visited += ((px, py))
          queue.enqueue((px, py, steps + 1))
        }
      }
    }
    -1
  }

  def main(args: Array[String]): Unit = {
    val maze = readMaze("input.txt")
    val (portals, portalPositions) = findPortals(maze)
    val start = portals("AA").head
    val end = portals("ZZ").head
    val result = bfs(maze, portals, portalPositions, start, end)
    println(result)
  }
}
