
import scala.io.Source
import scala.collection.mutable

object Day22 {

  case class Brick(id: Int, x1: Int, y1: Int, z1: Int, x2: Int, y2: Int, z2: Int)

  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val lines = Source.fromFile(filename).getLines().toList
    val bricks = parseBricks(lines)
    val (settledBricks, supports, supportedBy) = settleBricks(bricks)

    val part1Result = countSafeToDisintegrate(settledBricks, supports, supportedBy)
    println(s"Part 1: $part1Result")

    val part2Result = sumChainReactionFalls(settledBricks, supports, supportedBy)
    println(s"Part 2: $part2Result")

  }

  def parseBricks(lines: List[String]): List[Brick] = {
    lines.zipWithIndex.map { case (line, index) =>
      val parts = line.split("~")
      val startCoords = parts(0).split(",").map(_.toInt)
      val endCoords = parts(1).split(",").map(_.toInt)
      Brick(index, startCoords(0), startCoords(1), startCoords(2), endCoords(0), endCoords(1), endCoords(2))
    }
  }

  def settleBricks(bricks: List[Brick]): (List[Brick], Map[Int, Set[Int]], Map[Int, Set[Int]]) = {
    val sortedBricks = bricks.sortBy(_.z1)
    val settledBricks = mutable.ListBuffer[Brick]()
    val supports = mutable.Map[Int, Set[Int]]()
    val supportedBy = mutable.Map[Int, Set[Int]]()

    for (brick <- sortedBricks) {
      var maxZ = 1
      val supportingBricks = mutable.Set[Int]()

      for (settledBrick <- settledBricks) {
        if (overlap(brick, settledBrick)) {
          if (settledBrick.z2 + 1 > maxZ) {
            maxZ = settledBrick.z2 + 1
            supportingBricks.clear()
            supportingBricks += settledBrick.id
          } else if (settledBrick.z2 + 1 == maxZ) {
            supportingBricks += settledBrick.id
          }
        }
      }

      val newZ1 = maxZ
      val newZ2 = newZ1 + (brick.z2 - brick.z1)
      val newBrick = brick.copy(z1 = newZ1, z2 = newZ2)
      settledBricks += newBrick

      supports(newBrick.id) = Set()
      supportedBy(newBrick.id) = supportingBricks.toSet

      for (supportingBrickId <- supportingBricks) {
        supports(supportingBrickId) = supports.getOrElse(supportingBrickId, Set()) + newBrick.id
      }
    }
    (settledBricks.toList, supports.toMap, supportedBy.toMap)
  }


  def overlap(b1: Brick, b2: Brick): Boolean = {
    math.max(b1.x1, b2.x1) <= math.min(b1.x2, b2.x2) &&
      math.max(b1.y1, b2.y1) <= math.min(b1.y2, b2.y2)
  }

  def countSafeToDisintegrate(bricks: List[Brick], supports: Map[Int, Set[Int]], supportedBy: Map[Int, Set[Int]]): Int = {
    bricks.count { brick =>
      supports.getOrElse(brick.id, Set()).forall { supportedBrickId =>
        supportedBy.getOrElse(supportedBrickId, Set()).size > 1
      }
    }
  }


  def sumChainReactionFalls(bricks: List[Brick], supports: Map[Int, Set[Int]], supportedBy: Map[Int, Set[Int]]): Int = {
    bricks.map { brick =>
      val falling = mutable.Queue[Int](brick.id)
      val fallen = mutable.Set[Int](brick.id)

      while (falling.nonEmpty) {
        val current = falling.dequeue()
        for (supportedBrickId <- supports.getOrElse(current, Set())) {
          if (!fallen.contains(supportedBrickId)) {
            val remainingSupports = supportedBy(supportedBrickId) -- fallen
            if (remainingSupports.isEmpty) {
              falling.enqueue(supportedBrickId)
              fallen += supportedBrickId
            }
          }
        }
      }
      fallen.size - 1 // Exclude the disintegrated brick itself
    }.sum
  }
}
