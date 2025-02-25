
import scala.collection.mutable

object Day17 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input.txt").getLines().mkString
    val part1Result = solve(input, 2022)
    println(s"Part 1: $part1Result")

    val part2Result = solve(input, 1000000000000L)
    println(s"Part 2: $part2Result")
  }

  def solve(jetPattern: String, totalRocks: Long): Long = {
    val rocks: Vector[Vector[(Int, Int)]] = Vector(
      Vector((0, 0), (1, 0), (2, 0), (3, 0)),      // -
      Vector((1, 0), (0, 1), (1, 1), (2, 1), (1, 2)), // +
      Vector((0, 0), (1, 0), (2, 0), (2, 1), (2, 2)), //  উল্টো L
      Vector((0, 0), (0, 1), (0, 2), (0, 3)),      // |
      Vector((0, 0), (1, 0), (0, 1), (1, 1))       // ■
    )

    val chamberWidth = 7
    var towerHeight = 0L
    var jetIndex = 0
    var rockIndex = 0
    val seen = mutable.Map[(Int, Int, String), (Long, Long)]() // (rockIndex, jetIndex, top profile) -> (rockCount, towerHeight)
    val chamber = mutable.Set[(Int, Int)]()
    var addedHeight = 0L
    var cycleFound= false


    var rockCount = 0L
    while (rockCount < totalRocks) {

        if(!cycleFound){
            val topProfile = getTopProfile(chamber, towerHeight.toInt, chamberWidth)
            val key = (rockIndex, jetIndex, topProfile)
            if (seen.contains(key)) {
                val (prevRockCount, prevTowerHeight) = seen(key)
                val cycleLength = rockCount - prevRockCount
                val remainingRocks = totalRocks - rockCount
                val numCycles = remainingRocks / cycleLength
                addedHeight = numCycles * (towerHeight - prevTowerHeight)
                rockCount += numCycles * cycleLength
                cycleFound = true
            } else {
                seen += (key -> (rockCount, towerHeight))
            }
        }

      var rock = rocks(rockIndex).map { case (x, y) => (x + 2, y + towerHeight.toInt + 3) }

      var stopped = false
      while (!stopped) {
        // Apply jet
        val jet = jetPattern(jetIndex)
        jetIndex = (jetIndex + 1) % jetPattern.length
        val movedRock = jet match {
          case '<' => rock.map { case (x, y) => (x - 1, y) }
          case '>' => rock.map { case (x, y) => (x + 1, y) }
        }

        if (movedRock.forall { case (x, y) => x >= 0 && x < chamberWidth && !chamber.contains((x, y)) }) {
          rock = movedRock
        }

        // Move down
        val fallenRock = rock.map { case (x, y) => (x, y - 1) }
        if (fallenRock.exists { case (x, y) => y < 0 } || fallenRock.exists(chamber.contains)) {
          chamber ++= rock
          towerHeight = towerHeight.max(rock.map(_._2).max + 1)
          stopped = true
        } else {
          rock = fallenRock
        }
      }

      rockIndex = (rockIndex + 1) % rocks.length
      rockCount += 1
    }

    towerHeight + addedHeight
  }


    def getTopProfile(chamber: mutable.Set[(Int, Int)], towerHeight: Int, width: Int): String = {
        val profile = for (x <- 0 until width) yield {
            (0 to 30).find(dy => chamber.contains((x, towerHeight - 1 - dy))) match {  // Limit profile depth for efficiency
                case Some(dy) => dy
                case None => -1
            }
        }
        profile.mkString(",")
    }
}
