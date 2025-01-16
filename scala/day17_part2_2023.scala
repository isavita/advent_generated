
import scala.collection.mutable
import scala.io.Source

object Solution {

  case class Coord(x: Int, y: Int) {
    def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
    def -(other: Coord): Coord = Coord(x - other.x, y - other.y)
    def opposite: Coord = Coord(-x, -y)
  }

  case class Grid(width: Int, height: Int, data: Map[Coord, Int]) {
    def neighbors4(coord: Coord): Seq[Coord] = {
      val directions = Seq(Coord(0, -1), Coord(-1, 0), Coord(0, 1), Coord(1, 0))
      directions.map(coord + _).filter(c => 0 <= c.x && c.x < width && 0 <= c.y && c.y < height)
    }
  }

  case class Info(coord: Coord, dir: Coord, numStraight: Int)

  def heuristic(c1: Coord, c2: Coord): Int = (c1.x - c2.x).abs + (c1.y - c2.y).abs

  def aStarConstrained(grid: Grid, start: Coord, goal: Coord, minStraight: Int, maxStraight: Int): Int = {
    val startInfo = Info(start, Coord(0, 0), 0)
    val frontier = mutable.PriorityQueue.empty[(Info, Int)](Ordering.by(-_._2))
    frontier.enqueue((startInfo, 0))
    val cameFrom = mutable.Map.empty[Info, Info]
    val costSoFar = mutable.Map.empty[Info, Int]
    cameFrom(startInfo) = startInfo
    costSoFar(startInfo) = 0

    while (frontier.nonEmpty) {
      val (current, _) = frontier.dequeue()
      val currentCost = costSoFar(current)

      if (current.coord == goal) {
        return currentCost
      }

      for (next <- grid.neighbors4(current.coord)) {
        val newDir = next - current.coord
        val newNumStraight = if (newDir == current.dir) current.numStraight + 1 else 1
        val nextInfo = Info(next, newDir, newNumStraight)
        val newCost = currentCost + grid.data(next)

        val isLowerCost = !costSoFar.contains(nextInfo) || newCost < costSoFar(nextInfo)
        val isValidStraight = (current.numStraight >= minStraight || newDir == current.dir || current.coord == start) &&
          (newNumStraight <= maxStraight)
        val isNotOppositeDirection = newDir != current.dir.opposite

        if (isLowerCost && isValidStraight && isNotOppositeDirection) {
          costSoFar(nextInfo) = newCost
          cameFrom(nextInfo) = current
          val priority = newCost + heuristic(next, goal)
          frontier.enqueue((nextInfo, priority))
        }
      }
    }
    -1
  }

  def solve(input: Seq[String]): Int = {
    val grid = Grid(
      input.head.length,
      input.length,
      (for {
        (line, y) <- input.zipWithIndex
        (char, x) <- line.zipWithIndex
      } yield Coord(x, y) -> (char - '0')).toMap
    )
    val start = Coord(0, 0)
    val goal = Coord(grid.width - 1, grid.height - 1)
    aStarConstrained(grid, start, goal, 4, 10)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toSeq
    println(solve(input))
  }
}
