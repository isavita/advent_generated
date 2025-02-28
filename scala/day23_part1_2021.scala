
import scala.collection.mutable
import scala.io.Source

object Amphipod {

  case class State(grid: Vector[Vector[Char]], energyUsed: Int, path: String) extends Ordered[State] {
    def compare(that: State): Int = this.energyUsed compare that.energyUsed

    def copy(newGrid: Vector[Vector[Char]] = grid, newEnergyUsed: Int = energyUsed, newPath: String = path): State =
      State(newGrid, newEnergyUsed, newPath)

    def allDone(roomCoordToWantChar: Map[(Int, Int), Char]): Boolean =
      roomCoordToWantChar.forall { case ((r, c), want) => grid(r)(c) == want }

    def getUnsettledCoords(roomCoordToWantChar: Map[(Int, Int), Char]): List[(Int, Int)] = {
      val unsettled = mutable.ListBuffer[(Int, Int)]()

      for (c <- 1 until grid(0).length) {
        if (grid(1).slice(c, c + 1).exists("ABCD".contains(_))) {
          unsettled += ((1, c))
        }
      }

      for (c <- List(3, 5, 7, 9)) {
        var roomFullFromBack = true
        for (r <- grid.length - 2 to 2 by -1) {
          val coord = (r, c)
          val wantChar = roomCoordToWantChar.get(coord)
          val gotChar = grid(r)(c)
          if (gotChar != '.') {
            if (gotChar != wantChar.getOrElse(' ')) {
              roomFullFromBack = false
              unsettled += coord
            } else if (gotChar == wantChar.get && !roomFullFromBack) {
              unsettled += coord
            }
          }
        }
      }
      unsettled.toList
    }

    def getNextPossibleMoves(
        unsettledCoord: (Int, Int),
        roomCoordToWantChar: Map[(Int, Int), Char]
    ): List[(Int, Int)] = {
      val unsettledChar = grid(unsettledCoord._1)(unsettledCoord._2)
      val startedInHallway = unsettledCoord._1 == 1
      val possible = mutable.ListBuffer[(Int, Int)]()
      val queue = mutable.Queue(unsettledCoord)
      val seen = mutable.Set(unsettledCoord)

      while (queue.nonEmpty) {
        val front = queue.dequeue()

        if (front != unsettledCoord) {
          if (!Set((1, 3), (1, 5), (1, 7), (1, 9)).contains(front)) {
            roomCoordToWantChar.get(front) match {
              case None =>
                if (!startedInHallway) possible += front
              case Some(wantChar) if wantChar == unsettledChar =>
                var isStuckAmphipod = false
                var roomHasDeeperOpenSpaces = false
                for (r <- front._1 + 1 until grid.length - 1) {
                  val char = grid(r)(front._2)
                  if (char == '.') roomHasDeeperOpenSpaces = true
                  else if (char != unsettledChar) {
                    isStuckAmphipod = true
                  }
                }
                if (!roomHasDeeperOpenSpaces && !isStuckAmphipod) possible += front
              case _ =>
            }
          }
        }

        for ((dr, dc) <- List((-1, 0), (1, 0), (0, -1), (0, 1))) {
          val (nextRow, nextCol) = (front._1 + dr, front._2 + dc)
          if (
            nextRow >= 0 && nextRow < grid.length && nextCol >= 0 && nextCol < grid(0).length &&
            grid(nextRow)(nextCol) == '.' && !seen.contains((nextRow, nextCol))
          ) {
            queue.enqueue((nextRow, nextCol))
            seen += ((nextRow, nextCol))
          }
        }
      }
      possible.toList
    }
  }

  def parseInput(inputStr: String): State =
    State(inputStr.split("\n").map(_.toVector).toVector, 0, "")

  def calcEnergy(char: Char, start: (Int, Int), end: (Int, Int)): Int = {
    val dist = math.abs(end._2 - start._2) + (start._1 - 1) + (end._1 - 1)
    val energyPerType = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
    energyPerType(char) * dist
  }

  def amphipod(inputStr: String): Int = {
    val start = parseInput(inputStr)
    val roomCoordToWantChar = Map(
      (2, 3) -> 'A',
      (3, 3) -> 'A',
      (2, 5) -> 'B',
      (3, 5) -> 'B',
      (2, 7) -> 'C',
      (3, 7) -> 'C',
      (2, 9) -> 'D',
      (3, 9) -> 'D'
    )

    val minHeap = mutable.PriorityQueue(start)(Ordering[State].reverse)
    val seenGrids = mutable.Set[String]()

    while (minHeap.nonEmpty) {
      val front = minHeap.dequeue()
      val key = front.grid.map(_.mkString).mkString
      if (!seenGrids.contains(key)) {
        seenGrids += key

        if (front.allDone(roomCoordToWantChar)) {
          return front.energyUsed
        }

        val unsettledCoords = front.getUnsettledCoords(roomCoordToWantChar)
        for (unsettledCoord <- unsettledCoords) {
          val (ur, uc) = unsettledCoord
          val nextMoves = front.getNextPossibleMoves(unsettledCoord, roomCoordToWantChar)
          for (nextCoord <- nextMoves) {
            val (nr, nc) = nextCoord
            val cp = front.copy(
              newGrid = front.grid.updated(nr, front.grid(nr).updated(nc, front.grid(ur)(uc))).updated(ur, front.grid(ur).updated(uc, '.')),
              newEnergyUsed = front.energyUsed + calcEnergy(front.grid(ur)(uc), unsettledCoord, nextCoord),
              newPath =
                front.path + s"${front.grid(ur)(uc)}$unsettledCoord->$nextCoord(${front.energyUsed + calcEnergy(front.grid(ur)(uc), unsettledCoord, nextCoord)}),"
            )

            minHeap.enqueue(cp)
          }
        }
      }
    }
    throw new IllegalArgumentException("No solution found")
  }

  def main(args: Array[String]): Unit = {
    val inputStr = Source.fromFile("input.txt").getLines().mkString("\n").trim
    println(amphipod(inputStr))
  }
}
