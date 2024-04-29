import scala.io.Source
import scala.collection.mutable

object HillClimbingAlgorithm {
  case class Point(x: Int, y: Int, elevation: Char)
  case class QueueItem(point: Point, distance: Int)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toArray
    val heightmap = lines.map(_.toCharArray)
    val queue = new mutable.Queue[QueueItem]()
    val visited = Array.fill(lines.length, lines.head.length)(false)

    var startX = 0
    var startY = 0
    var endX = 0
    var endY = 0

    for (i <- heightmap.indices; j <- heightmap(i).indices) {
      if (heightmap(i)(j) == 'S') {
        startX = j
        startY = i
      } else if (heightmap(i)(j) == 'E') {
        endX = j
        endY = i
      }
    }

    queue.enqueue(QueueItem(Point(startX, startY, 'a'), 0))
    visited(startY)(startX) = true

    while (queue.nonEmpty) {
      val QueueItem(point, distance) = queue.dequeue()
      if (point.x == endX && point.y == endY) {
        println(distance)
        return
      }

      val directions = List((0, 1), (0, -1), (1, 0), (-1, 0))
      for ((dx, dy) <- directions) {
        val newX = point.x + dx
        val newY = point.y + dy
        if (newX >= 0 && newX < heightmap.head.length && newY >= 0 && newY < heightmap.length) {
          val newElevation = heightmap(newY)(newX)
          if (!visited(newY)(newX) && canClimb(point.elevation, newElevation)) {
            queue.enqueue(QueueItem(Point(newX, newY, newElevation), distance + 1))
            visited(newY)(newX) = true
          }
        }
      }
    }
  }

  def canClimb(from: Char, to: Char): Boolean = {
    val fromInt = from - 'a'
    val toInt = if (to == 'E') 25 else to - 'a'
    toInt - fromInt <= 1
  }
}