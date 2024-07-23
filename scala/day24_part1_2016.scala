
import scala.io.Source
import scala.collection.mutable

object AirDuctSpelunking {
  
  case class Point(x: Int, y: Int)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toArray
    val (map, points) = parseMap(lines)
    val distances = computeDistances(map, points)
    val shortestPath = findShortestPath(distances)
    println(shortestPath)
  }

  def parseMap(lines: Array[String]): (Array[Array[Char]], Map[Int, Point]) = {
    val map = lines.map(_.toCharArray)
    val points = mutable.Map[Int, Point]()

    for (y <- lines.indices; x <- lines(y).indices) {
      map(y)(x) match {
        case '.' | '#' => // Do nothing
        case c if c.isDigit => points(c.asDigit) = Point(x, y)
        case _ => // Do nothing
      }
    }

    (map, points.toMap)
  }

  def computeDistances(map: Array[Array[Char]], points: Map[Int, Point]): Map[(Int, Int), Int] = {
    val distances = mutable.Map[(Int, Int), Int]()

    for ((id, start) <- points) {
      val distanceMap = bfs(map, start)
      for ((targetId, target) <- points if targetId != id) {
        distances((id, targetId)) = distanceMap.getOrElse(target, Int.MaxValue)
      }
    }

    distances.toMap
  }

  def bfs(map: Array[Array[Char]], start: Point): Map[Point, Int] = {
    val directions = Array(Point(0, 1), Point(1, 0), Point(0, -1), Point(-1, 0))
    val queue = mutable.Queue[(Point, Int)]((start, 0))
    val visited = mutable.Set[Point]()
    val distanceMap = mutable.Map[Point, Int]()

    while (queue.nonEmpty) {
      val (current, dist) = queue.dequeue()
      if (!visited.contains(current)) {
        visited.add(current)
        distanceMap(current) = dist

        for (dir <- directions) {
          val next = Point(current.x + dir.x, current.y + dir.y)
          if (isValid(map, next) && !visited.contains(next)) {
            queue.enqueue((next, dist + 1))
          }
        }
      }
    }

    distanceMap.toMap
  }

  def isValid(map: Array[Array[Char]], point: Point): Boolean = {
    point.x >= 0 && point.x < map(0).length && point.y >= 0 && point.y < map.length && (map(point.y)(point.x) == '.' || map(point.y)(point.x).isDigit)
  }

  def findShortestPath(distances: Map[(Int, Int), Int]): Int = {
    val allPoints = distances.keys.flatMap { case (a, b) => Seq(a, b) }.toSet
    val start = 0
    val remaining = allPoints - start
    val memo = mutable.Map[(Int, Set[Int]), Int]()

    def tsp(current: Int, visited: Set[Int]): Int = {
      if (visited.size == remaining.size) return 0
      if (memo.contains((current, visited))) return memo((current, visited))

      val result = remaining.toSeq.map { next =>
        if (!visited.contains(next)) {
          distances.get((current, next)).getOrElse(Int.MaxValue) + tsp(next, visited + next)
        } else {
          Int.MaxValue
        }
      }.min

      memo((current, visited)) = result
      result
    }

    tsp(start, Set())
  }
}
