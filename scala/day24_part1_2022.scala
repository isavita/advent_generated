
import scala.collection.mutable
import scala.io.Source

object Day24 {

  case class Point(x: Int, y: Int) {
    def +(other: Point): Point = Point(x + other.x, y + other.y)
  }

  val directions: Seq[Point] = Seq(Point(0, 1), Point(1, 0), Point(0, -1), Point(-1, 0), Point(0, 0))

  case class Blizzard(pos: Point, dir: Point)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toList
    val (blizzards, width, height) = parseInput(lines)
    val start = Point(1, 0)
    val end = Point(width - 2, height - 1)

    val minutes = bfs(start, end, blizzards, width, height, 0)
    println(minutes)

     //part 2. Go back to start and then to the end again.
    val minutesBackToStart = bfs(end, start, blizzards, width, height, minutes)
    val minutesToGoalAgain = bfs(start, end, blizzards, width, height, minutesBackToStart)
    println(minutesToGoalAgain)
  }


  def parseInput(lines: List[String]): (Set[Blizzard], Int, Int) = {
    val blizzards = lines.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.flatMap { case (cell, x) =>
        cell match {
          case '>' => Some(Blizzard(Point(x, y), Point(1, 0)))
          case '<' => Some(Blizzard(Point(x, y), Point(-1, 0)))
          case 'v' => Some(Blizzard(Point(x, y), Point(0, 1)))
          case '^' => Some(Blizzard(Point(x, y), Point(0, -1)))
          case _ => None
        }
      }
    }.toSet

    (blizzards, lines.head.length, lines.length)
  }

  def bfs(start: Point, end: Point, initialBlizzards: Set[Blizzard], width: Int, height: Int, startTime: Int): Int = {
    val queue = mutable.Queue[(Point, Int)]()
    val visited = mutable.Set[(Point, Int)]()

    queue.enqueue((start, startTime))
    visited.add((start, startTime))

    while (queue.nonEmpty) {
      val (currentPos, currentTime) = queue.dequeue()

      if (currentPos == end) {
        return currentTime
      }

      val nextTime = currentTime + 1
      val blizzardPositions = getBlizzardPositions(initialBlizzards, width, height, nextTime)

      for (dir <- directions) {
        val nextPos = currentPos + dir
        if (isValidPosition(nextPos, width, height) && !blizzardPositions.contains(nextPos)) {
          if (!visited.contains((nextPos, nextTime))) {
            visited.add((nextPos, nextTime))
            queue.enqueue((nextPos, nextTime))
          }
        }
      }
    }
    -1 // Should not happen if a path exists
  }
  
  def getBlizzardPositions(initialBlizzards: Set[Blizzard], width: Int, height: Int, time: Int): Set[Point] = {
        initialBlizzards.map { blizzard =>
            val wrappedX = (blizzard.pos.x - 1 + blizzard.dir.x * time) % (width - 2)
            val finalX = if (wrappedX < 0) wrappedX + width - 2 + 1 else wrappedX + 1
            val wrappedY = (blizzard.pos.y - 1 + blizzard.dir.y * time) % (height - 2)
            val finalY = if(wrappedY < 0) wrappedY + height - 2 + 1 else wrappedY + 1
            Point(finalX, finalY)
        }
  }

  def isValidPosition(pos: Point, width: Int, height: Int): Boolean = {
    (pos.x > 0 && pos.x < width - 1 && pos.y > 0 && pos.y < height - 1) ||
      (pos.x == 1 && pos.y == 0) || (pos.x == width - 2 && pos.y == height -1)
  }
}
