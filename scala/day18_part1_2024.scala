
import scala.io.Source
import scala.collection.mutable

object RAMRun {
  def main(args: Array[String]): Unit = {
    val inputFile = "input.txt"
    val gridSize = 70
    val memorySpace = Array.ofDim[Boolean](gridSize + 1, gridSize + 1)

    // Read input from file
    val input = Source.fromFile(inputFile).getLines().toList

    // Simulate the falling bytes
    input.take(1024).foreach { line =>
      val Array(x, y) = line.split(",").map(_.toInt)
      memorySpace(x)(y) = true
    }

    // Find the shortest path using BFS
    val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))
    val queue = mutable.Queue((0, 0, 0)) // (x, y, steps)
    val visited = mutable.Set((0, 0))

    while (queue.nonEmpty) {
      val (x, y, steps) = queue.dequeue()

      if (x == gridSize && y == gridSize) {
        println(s"The minimum number of steps needed to reach the exit is $steps")
        return
      }

      for ((dx, dy) <- directions) {
        val nx = x + dx
        val ny = y + dy

        if (nx >= 0 && nx <= gridSize && ny >= 0 && ny <= gridSize && !memorySpace(nx)(ny) && !visited((nx, ny))) {
          visited += ((nx, ny))
          queue.enqueue((nx, ny, steps + 1))
        }
      }
    }

    println("No path found to the exit.")
  }
}
