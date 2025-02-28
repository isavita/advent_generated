
import scala.io.Source
import scala.collection.mutable
import scala.util.Using

object BlizzardBasin {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("input.txt")) { source =>
      val lines = source.getLines().toVector
      val (grid, width, height) = parseGrid(lines)

      val start = (1, 0)
      val goal = (width - 2, height - 1)

      val trip1 = findShortestPath(grid, width, height, start, goal)
      println(s"Part 1: $trip1")

      val trip2 = findShortestPath(grid, width, height, goal, start, trip1)
      val trip3 = findShortestPath(grid, width, height, start, goal, trip1 + trip2)

      println(s"Part 2: ${trip1 + trip2 + trip3}")
    }.get
  }

  def parseGrid(lines: Vector[String]): (Vector[String], Int, Int) = {
    val width = lines.head.length
    val height = lines.length
    (lines, width, height)
  }

  def findShortestPath(
      initialGrid: Vector[String],
      width: Int,
      height: Int,
      start: (Int, Int),
      goal: (Int, Int),
      startTime: Int = 0
  ): Int = {
    val visited = mutable.Set[(Int, Int, Int)]()
    val queue = mutable.Queue[(Int, Int, Int)]((start._1, start._2, startTime))
    visited.add((start._1, start._2, startTime))

    while (queue.nonEmpty) {
      val (x, y, time) = queue.dequeue()

      if ((x, y) == goal) {
        return time - startTime
      }

      val nextTime = time + 1
      val nextGrid = calculateBlizzardPositions(initialGrid, width, height, nextTime)

      val possibleMoves = Seq((x, y), (x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1))

      for ((nextX, nextY) <- possibleMoves) {
        if (
          nextX >= 1 && nextX < width - 1 && nextY >= 1 && nextY < height - 1 &&
          nextGrid(nextY)(nextX) == '.' && !visited.contains((nextX, nextY, nextTime))
        ) {
          queue.enqueue((nextX, nextY, nextTime))
          visited.add((nextX, nextY, nextTime))
        } else if ((nextX, nextY) == goal && !visited.contains((nextX, nextY, nextTime))) {
          queue.enqueue((nextX, nextY, nextTime))
          visited.add((nextX, nextY, nextTime))
        } else if ((nextX, nextY) == (1,0) && !visited.contains((nextX, nextY, nextTime))) {
          queue.enqueue((nextX, nextY, nextTime))
          visited.add((nextX, nextY, nextTime))
        }

      }
    }
    -1 // Should not happen if a path exists
  }

  def calculateBlizzardPositions(
      initialGrid: Vector[String],
      width: Int,
      height: Int,
      time: Int
  ): Vector[String] = {
    val newGrid = Array.fill(height)(Array.fill(width)('.'))
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        if (initialGrid(y)(x) == '#') {
          newGrid(y)(x) = '#'
        }
      }
    }

    for (y <- 0 until height; x <- 0 until width) {
      initialGrid(y)(x) match {
        case '>' =>
          val newX = ((x - 1 + time) % (width - 2) + (width - 2)) % (width - 2) + 1
          if (newGrid(y)(newX) == '.') newGrid(y)(newX) = '>' else if (newGrid(y)(newX).isDigit) newGrid(y)(newX) = (newGrid(y)(newX).asDigit + 1).toString.head else newGrid(y)(newX) = '2'
        case '<' =>
          val newX = ((x - 1 - time) % (width - 2) + (width - 2)) % (width - 2) + 1
           if (newGrid(y)(newX) == '.') newGrid(y)(newX) = '<' else if (newGrid(y)(newX).isDigit) newGrid(y)(newX) = (newGrid(y)(newX).asDigit + 1).toString.head else newGrid(y)(newX) = '2'
        case '^' =>
          val newY = ((y - 1 - time) % (height - 2) + (height - 2)) % (height - 2) + 1
           if (newGrid(newY)(x) == '.') newGrid(newY)(x) = '^' else if (newGrid(newY)(x).isDigit) newGrid(newY)(x) = (newGrid(newY)(x).asDigit + 1).toString.head else newGrid(newY)(x) = '2'
        case 'v' =>
          val newY = ((y - 1 + time) % (height - 2) + (height - 2)) % (height - 2) + 1
           if (newGrid(newY)(x) == '.') newGrid(newY)(x) = 'v' else if (newGrid(newY)(x).isDigit) newGrid(newY)(x) = (newGrid(newY)(x).asDigit + 1).toString.head else newGrid(newY)(x) = '2'
        case _ =>
      }
    }

    newGrid.map(_.mkString).toVector
  }
}
