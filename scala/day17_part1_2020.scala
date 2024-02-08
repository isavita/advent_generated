
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.toList

  def countActiveCubes(input: List[String], cycles: Int): Int = {
    val initialGrid = input.map(_.toList)
    val initialState = initialGrid.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.collect {
        case ('#', x) => (x, y, 0)
      }
    }.toSet

    def neighbors(x: Int, y: Int, z: Int): Set[(Int, Int, Int)] = {
      (for {
        dx <- -1 to 1
        dy <- -1 to 1
        dz <- -1 to 1
        if !(dx == 0 && dy == 0 && dz == 0)
      } yield (x + dx, y + dy, z + dz)).toSet
    }

    def countActiveNeighbors(x: Int, y: Int, z: Int, activeCubes: Set[(Int, Int, Int)]): Int = {
      neighbors(x, y, z).count(activeCubes.contains)
    }

    def nextCycle(activeCubes: Set[(Int, Int, Int)]): Set[(Int, Int, Int)] = {
      val inactiveNeighbors = activeCubes.flatMap { case (x, y, z) =>
        neighbors(x, y, z).filterNot(activeCubes.contains)
      }

      val nextActiveCubes = activeCubes.filter { cube =>
        val activeNeighbours = countActiveNeighbors(cube._1, cube._2, cube._3, activeCubes)
        activeNeighbours == 2 || activeNeighbours == 3
      }

      val nextInactiveCubes = inactiveNeighbors.filter { cube =>
        countActiveNeighbors(cube._1, cube._2, cube._3, activeCubes) == 3
      }

      nextActiveCubes ++ nextInactiveCubes
    }

    def runCycles(activeCubes: Set[(Int, Int, Int)], n: Int): Set[(Int, Int, Int)] = {
      if (n == 0) activeCubes
      else runCycles(nextCycle(activeCubes), n - 1)
    }

    runCycles(initialState, cycles).size
  }

  println(countActiveCubes(input, 6))
}
