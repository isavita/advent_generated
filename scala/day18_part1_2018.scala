object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList
  val grid = input.map(_.toCharArray)

  def countAdjacent(x: Int, y: Int, grid: List[Array[Char]], acre: Char): Int = {
    val adjacent = List(
      (-1, -1), (-1, 0), (-1, 1),
      (0, -1), (0, 1),
      (1, -1), (1, 0), (1, 1)
    )

    adjacent.count { case (dx, dy) =>
      val newX = x + dx
      val newY = y + dy
      newX >= 0 && newX < grid.length && newY >= 0 && newY < grid(0).length && grid(newX)(newY) == acre
    }
  }

  def nextMinute(grid: List[Array[Char]]): List[Array[Char]] = {
    grid.zipWithIndex.map { case (row, x) =>
      row.zipWithIndex.map { case (acre, y) =>
        val adjTrees = countAdjacent(x, y, grid, '|')
        val adjLumberyards = countAdjacent(x, y, grid, '#')

        acre match {
          case '.' => if (adjTrees >= 3) '|' else '.'
          case '|' => if (adjLumberyards >= 3) '#' else '|'
          case '#' => if (adjTrees >= 1 && adjLumberyards >= 1) '#' else '.'
        }
      }
    }
  }

  def resourceValue(grid: List[Array[Char]]): Int = {
    val wooded = grid.flatten.count(_ == '|')
    val lumberyards = grid.flatten.count(_ == '#')
    wooded * lumberyards
  }

  val finalGrid = (1 to 10).foldLeft(grid) { case (acc, _) => nextMinute(acc) }
  println(resourceValue(finalGrid))
}