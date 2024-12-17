
import scala.io.Source
import scala.util.Using
import scala.collection.mutable
import scala.math.min

object Solution {

  def main(args: Array[String]): Unit = {
    val input = readFile("input.txt")
    val result = cleaningRobot(input)
    println(result)
  }

  def cleaningRobot(input: String): Int = {
    val grid = input.linesIterator.map(_.toArray).toArray
    val pois = mutable.Map[Int, (Int, Int)]()
    for {
      r <- grid.indices
      c <- grid(r).indices
      if grid(r)(c).isDigit
    } {
      pois(grid(r)(c).asDigit) = (r, c)
    }

    val graph = Array.ofDim[Int](pois.size, pois.size)
    for ((poi, (r, c)) <- pois) {
      val distances = bfsGetEdgeWeights(grid, r, c, pois)
      graph(poi) = distances
    }

    dfs(graph, 0, Set(0), true)
  }

  case class Node(row: Int, col: Int, distance: Int)

  def bfsGetEdgeWeights(grid: Array[Array[Char]], startRow: Int, startCol: Int, pois: mutable.Map[Int, (Int, Int)]): Array[Int] = {
    val poiToDistance = mutable.Map(grid(startRow)(startCol).asDigit -> 0)
    val queue = mutable.Queue(Node(startRow, startCol, 0))
    val visited = mutable.Set[(Int, Int)]((startRow, startCol))

    while (queue.nonEmpty) {
      val front = queue.dequeue()

      if (grid(front.row)(front.col).isDigit && !poiToDistance.contains(grid(front.row)(front.col).asDigit)) {
        poiToDistance(grid(front.row)(front.col).asDigit) = front.distance
      }

      val dirs = Array((0, -1), (0, 1), (1, 0), (-1, 0))
      for ((dr, dc) <- dirs) {
        val nextRow = front.row + dr
        val nextCol = front.col + dc
        if (nextRow >= 0 && nextRow < grid.length && nextCol >= 0 && nextCol < grid(0).length && grid(nextRow)(nextCol) != '#' && !visited.contains((nextRow, nextCol))) {
          queue.enqueue(Node(nextRow, nextCol, front.distance + 1))
          visited.add((nextRow, nextCol))
        }
      }
    }

    val distances = Array.ofDim[Int](pois.size)
    for ((num, dist) <- poiToDistance) {
      distances(num) = dist
    }
    distances
  }

  def dfs(graph: Array[Array[Int]], entryIndex: Int, visited: Set[Int], returnToZero: Boolean): Int = {
    if (graph.length == visited.size) {
      if (returnToZero) graph(entryIndex)(0) else 0
    } else {
      var minDistance = Int.MaxValue
      for ((dist, i) <- graph(entryIndex).zipWithIndex) {
        if (!visited.contains(i)) {
          minDistance = min(minDistance, dist + dfs(graph, i, visited + i, returnToZero))
        }
      }
      minDistance
    }
  }

  def readFile(path: String): String = {
    Using(Source.fromFile(path))(_.getLines().mkString("\n")).get
  }
}
