
import scala.io.Source

object Day22 {

  case class Node(x: Int, y: Int, size: Int, used: Int, avail: Int)

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    val nodes = parseNodes(input)

    println(s"Part 1: ${countViablePairs(nodes)}")
    println(s"Part 2: ${findShortestPath(nodes)}")
  }

  def parseNodes(input: List[String]): List[Node] = {
    input.drop(2).map { line =>
      val parts = line.split("\\s+").filter(_.nonEmpty)
      val Array(x, y) = parts(0).split("-").tail.map(_.drop(1).toInt)
      val size = parts(1).dropRight(1).toInt
      val used = parts(2).dropRight(1).toInt
      val avail = parts(3).dropRight(1).toInt
      Node(x, y, size, used, avail)
    }
  }

  def countViablePairs(nodes: List[Node]): Int = {
    (for {
      a <- nodes
      b <- nodes
      if a.used != 0 && a != b && a.used <= b.avail
    } yield (a, b)).size
  }

  def findShortestPath(nodes: List[Node]): Int = {
    val maxX = nodes.map(_.x).max
    val maxY = nodes.map(_.y).max
    val goal = (maxX, 0)
    val start = (0, 0)
    val emptyNode = nodes.find(_.used == 0).get

    val walls = nodes.filter(n => n.used > emptyNode.size).map(n => (n.x, n.y)).toSet

    // Find the shortest path for the empty node to reach the node left to the goal
    val emptyToGoalLeftPath = bfs(
      (emptyNode.x, emptyNode.y),
      (goal._1 - 1, goal._2),
      maxX,
      maxY,
      walls
    )

    // Calculate the number of steps to move the goal data to (0, 0)
    // After the empty node reaches the left of the goal, it takes 5 steps to move the goal one step to the left
    emptyToGoalLeftPath + (goal._1 - 1) * 5 + 1
  }

  def bfs(
      start: (Int, Int),
      target: (Int, Int),
      maxX: Int,
      maxY: Int,
      walls: Set[(Int, Int)]
  ): Int = {
    val q = scala.collection.mutable.Queue[(Int, Int)]()
    val visited = scala.collection.mutable.Set[(Int, Int)]()
    val dist = scala.collection.mutable.Map[(Int, Int), Int]()

    q.enqueue(start)
    visited.add(start)
    dist(start) = 0

    while (q.nonEmpty) {
      val curr = q.dequeue()
      if (curr == target) return dist(curr)

      val neighbors = List(
        (curr._1 - 1, curr._2),
        (curr._1 + 1, curr._2),
        (curr._1, curr._2 - 1),
        (curr._1, curr._2 + 1)
      )

      for (neighbor <- neighbors) {
        if (
          neighbor._1 >= 0 && neighbor._1 <= maxX &&
          neighbor._2 >= 0 && neighbor._2 <= maxY &&
          !walls.contains(neighbor) && !visited.contains(neighbor)
        ) {
          q.enqueue(neighbor)
          visited.add(neighbor)
          dist(neighbor) = dist(curr) + 1
        }
      }
    }
    -1 // Should not happen in this case
  }
}
