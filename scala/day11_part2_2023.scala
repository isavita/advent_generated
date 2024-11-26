
import scala.io.Source

object CosmicExpansion {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toArray
    val galaxies = findGalaxies(input)
    val expandedGalaxies = expandUniverse(galaxies, input, 1000000)
    val totalDistance = calculateTotalDistance(expandedGalaxies)
    println(totalDistance)
  }

  def findGalaxies(universe: Array[String]): List[(Int, Int)] = {
    universe.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.collect {
        case ('#', x) => (x, y)
      }
    }.toList
  }

  def expandUniverse(galaxies: List[(Int, Int)], universe: Array[String], expansionFactor: Long): List[(Long, Long)] = {
    val emptyRows = universe.indices.filterNot(y => galaxies.exists(_._2 == y))
    val emptyCols = universe(0).indices.filterNot(x => galaxies.exists(_._1 == x))

    galaxies.map { case (x, y) =>
      val expandedX = x + emptyCols.count(_ < x) * (expansionFactor - 1)
      val expandedY = y + emptyRows.count(_ < y) * (expansionFactor - 1)
      (expandedX.toLong, expandedY.toLong)
    }
  }

  def calculateTotalDistance(galaxies: List[(Long, Long)]): Long = {
    galaxies.indices.flatMap { i =>
      (i + 1 until galaxies.length).map { j =>
        val (x1, y1) = galaxies(i)
        val (x2, y2) = galaxies(j)
        math.abs(x1 - x2) + math.abs(y1 - y2)
      }
    }.sum
  }
}
