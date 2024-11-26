
import scala.io.Source

object PyroclasticFlow {
  // Rock shapes
  val rocks = List(
    List((0,0), (1,0), (2,0), (3,0)),                  // Horizontal line
    List((1,0), (0,1), (1,1), (2,1), (1,2)),           // Plus shape
    List((0,0), (1,0), (2,0), (2,1), (2,2)),           // L shape
    List((0,0), (0,1), (0,2), (0,3)),                  // Vertical line
    List((0,0), (1,0), (0,1), (1,1))                   // Square
  )

  def simulate(jets: String, rockCount: Int): Long = {
    var chamber = Set[(Int, Int)]()
    var height = 0
    var jetIndex = 0
    var rockIndex = 0

    def isBlocked(rock: List[(Int, Int)], dx: Int, dy: Int): Boolean = {
      rock.exists { case (x, y) =>
        val newX = x + dx
        val newY = y + dy
        newX < 0 || newX >= 7 || newY < 0 || chamber.contains((newX, newY))
      }
    }

    for (_ <- 1 to rockCount) {
      var rock = rocks(rockIndex).map { case (x, y) => (x + 2, y + height + 3) }
      rockIndex = (rockIndex + 1) % rocks.length

      var settled = false
      while (!settled) {
        // Jet push
        val jetDir = if (jets(jetIndex) == '<') -1 else 1
        jetIndex = (jetIndex + 1) % jets.length

        val pushedRock = rock.map { case (x, y) => (x + jetDir, y) }
        if (!isBlocked(pushedRock, 0, 0)) {
          rock = pushedRock
        }

        // Fall down
        val fallenRock = rock.map { case (x, y) => (x, y - 1) }
        if (isBlocked(fallenRock, 0, 0)) {
          settled = true
          rock.foreach(chamber += _)
          height = chamber.map(_._2).max + 1
        } else {
          rock = fallenRock
        }
      }
    }

    height
  }

  def main(args: Array[String]): Unit = {
    val jets = Source.fromFile("input.txt").mkString.trim
    val result = simulate(jets, 2022)
    println(s"Tower height after 2022 rocks: $result")
  }
}
