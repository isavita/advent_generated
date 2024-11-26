
import scala.collection.mutable
import scala.io.Source

object ClumsyCrucible {
  case class Point(x: Int, y: Int)
  case class State(point: Point, dir: Point, straightMoves: Int)

  def solve(grid: Vector[Vector[Int]]): Int = {
    val rows = grid.length
    val cols = grid.head.length
    val target = Point(cols - 1, rows - 1)

    val pq = mutable.PriorityQueue.empty[(Int, State)](
      Ordering.by(-_._1)
    )
    val seen = mutable.Set[State]()

    // Start in two possible initial directions
    pq.enqueue((0, State(Point(0, 0), Point(1, 0), 0)))
    pq.enqueue((0, State(Point(0, 0), Point(0, 1), 0)))

    while (pq.nonEmpty) {
      val (heatLoss, state) = pq.dequeue()
      val Point(x, y) = state.point

      if (x == target.x && y == target.y) return heatLoss

      if (seen.contains(state)) {
        // Skip if we've seen this exact state before
        pq.headOption
      } else {
        seen.add(state)

        // Possible moves: continue straight, turn left, turn right
        val moves = List(
          // Continue straight
          if (state.straightMoves < 3) 
            Some((state.dir.x, state.dir.y)) 
          else None,
          // Turn left
          Some((-state.dir.y, state.dir.x)),
          // Turn right
          Some((state.dir.y, -state.dir.x))
        ).flatten

        moves.foreach { (dx, dy) =>
          val newX = x + dx
          val newY = y + dy

          if (newX >= 0 && newX < cols && newY >= 0 && newY < rows) {
            val newHeatLoss = heatLoss + grid(newY)(newX)
            val newStraightMoves = 
              if (dx == state.dir.x && dy == state.dir.y) state.straightMoves + 1 
              else 1

            pq.enqueue((
              newHeatLoss, 
              State(Point(newX, newY), Point(dx, dy), newStraightMoves)
            ))
          }
        }
      }
    }

    -1 // No path found
  }

  def main(args: Array[String]): Unit = {
    val grid = Source.fromFile("input.txt")
      .getLines()
      .map(_.map(_.asDigit).toVector)
      .toVector

    val result = solve(grid)
    println(result)
  }
}
