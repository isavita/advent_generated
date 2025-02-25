
import scala.io.Source
import scala.collection.mutable
import scala.util.matching.Regex

object Nanobots {

  case class Nanobot(x: Int, y: Int, z: Int, r: Int)

  val pattern: Regex = "pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)".r

  def parseInput(filePath: String): List[Nanobot] = {
    Source.fromFile(filePath).getLines.flatMap { line =>
      pattern.findFirstMatchIn(line).map { m =>
        Nanobot(m.group(1).toInt, m.group(2).toInt, m.group(3).toInt, m.group(4).toInt)
      }
    }.toList
  }

  def manhattanDistance(a: (Int, Int, Int), b: (Int, Int, Int)): Int =
    (a._1 - b._1).abs + (a._2 - b._2).abs + (a._3 - b._3).abs

  def partOne(nanobots: List[Nanobot]): Int = {
    val strongest = nanobots.maxBy(_.r)
    nanobots.count { bot =>
      manhattanDistance((strongest.x, strongest.y, strongest.z), (bot.x, bot.y, bot.z)) <= strongest.r
    }
  }

  def minDistanceToOrigin(x: Int, y: Int, z: Int, size: Int): Int = {
    val dx = if (x > 0) x else if (x + size - 1 < 0) -(x + size - 1) else 0
    val dy = if (y > 0) y else if (y + size - 1 < 0) -(y + size - 1) else 0
    val dz = if (z > 0) z else if (z + size - 1 < 0) -(z + size - 1) else 0
    dx + dy + dz
  }

  def partTwo(nanobots: List[Nanobot]): Int = {
    val minX = nanobots.minBy(_.x).x
    val maxX = nanobots.maxBy(_.x).x
    val minY = nanobots.minBy(_.y).y
    val maxY = nanobots.maxBy(_.y).y
    val minZ = nanobots.minBy(_.z).z
    val maxZ = nanobots.maxBy(_.z).z

    var size = 1
    while (size < (maxX - minX).max((maxY - minY).max(maxZ - minZ))) {
      size *= 2
    }

    val heap = mutable.PriorityQueue.empty[(Int, Int, Int, Int, Int, Int)](
      Ordering.by(v => (-v._1, v._2, -v._3))
    )
    heap.enqueue((0, minDistanceToOrigin(minX, minY, minZ, size), size, minX, minY, minZ))

    var bestDistance = 0
    var bestCount = -1

    while (heap.nonEmpty) {
      val (negCount, distance, size, x, y, z) = heap.dequeue()
      val count = -negCount

      if (size == 1) {
        if (count > bestCount || (count == bestCount && distance < bestDistance)) {
          bestCount = count
          bestDistance = distance
        }
        return bestDistance
      }

      val half = size / 2
      for (dx <- Seq(0, half); dy <- Seq(0, half); dz <- Seq(0, half)) {
        val nx = x + dx
        val ny = y + dy
        val nz = z + dz
        val newSize = if (half < 1) 1 else half

        var count = 0
        for (bot <- nanobots) {
          var d = 0
          if (bot.x < nx) d += nx - bot.x
          else if (bot.x > nx + newSize - 1) d += bot.x - (nx + newSize - 1)
          if (bot.y < ny) d += ny - bot.y
          else if (bot.y > ny + newSize - 1) d += bot.y - (ny + newSize - 1)
          if (bot.z < nz) d += nz - bot.z
          else if (bot.z > nz + newSize - 1) d += bot.z - (nz + newSize - 1)

          if (d <= bot.r) count += 1
        }

        val newDistance = minDistanceToOrigin(nx, ny, nz, newSize)
        heap.enqueue((-count, newDistance, newSize, nx, ny, nz))
      }
    }
    bestDistance
  }


  def main(args: Array[String]): Unit = {
    val nanobots = parseInput("input.txt")
    println(s"Part One: ${partOne(nanobots)}")
    println(s"Part Two: ${partTwo(nanobots)}")
  }
}
