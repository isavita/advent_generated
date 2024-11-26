
object PlanetOfDiscord {
  type Grid = Vector[Vector[Char]]

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input.txt").getLines().toVector
    val initialGrid = input.map(_.toVector)
    
    val firstRepeatedLayout = findFirstRepeatedLayout(initialGrid)
    val biodiversityRating = calculateBiodiversityRating(firstRepeatedLayout)
    
    println(s"Biodiversity Rating: $biodiversityRating")
  }

  def findFirstRepeatedLayout(initialGrid: Grid): Grid = {
    var currentGrid = initialGrid
    val seenLayouts = scala.collection.mutable.Set[Grid]()

    while (!seenLayouts.contains(currentGrid)) {
      seenLayouts.add(currentGrid)
      currentGrid = simulateMinute(currentGrid)
    }

    currentGrid
  }

  def simulateMinute(grid: Grid): Grid = {
    grid.indices.map { row =>
      grid(row).indices.map { col =>
        val adjacentBugs = countAdjacentBugs(grid, row, col)
        grid(row)(col) match {
          case '#' if adjacentBugs != 1 => '.'
          case '.' if adjacentBugs == 1 || adjacentBugs == 2 => '#'
          case tile => tile
        }
      }.toVector
    }.toVector
  }

  def countAdjacentBugs(grid: Grid, row: Int, col: Int): Int = {
    val directions = List((0, 1), (0, -1), (1, 0), (-1, 0))
    directions.count { case (dx, dy) =>
      val newRow = row + dx
      val newCol = col + dy
      newRow >= 0 && newRow < grid.size && 
      newCol >= 0 && newCol < grid(0).size && 
      grid(newRow)(newCol) == '#'
    }
  }

  def calculateBiodiversityRating(grid: Grid): Int = {
    grid.indices.flatMap { row =>
      grid(row).indices.map { col =>
        if (grid(row)(col) == '#') math.pow(2, row * grid(row).size + col).toInt
        else 0
      }
    }.sum
  }
}
