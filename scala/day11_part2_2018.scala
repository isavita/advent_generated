
import scala.io.Source

object PowerGrid {
  def main(args: Array[String]): Unit = {
    val serial = Source.fromFile("input.txt").getLines().next().trim.toInt
    val gridSize = 300
    val grid = Array.ofDim[Int](gridSize, gridSize)

    for (y <- 0 until gridSize; x <- 0 until gridSize) {
      val rackID = x + 11
      val powerLevel = ((rackID * (y + 1) + serial) * rackID / 100) % 10 - 5
      grid(y)(x) = powerLevel
    }

    var (maxPower, maxX, maxY, maxSize) = (-Int.MaxValue, 0, 0, 0)

    for (size <- 1 to gridSize) {
      val sumGrid = Array.ofDim[Int](gridSize + 1, gridSize + 1)
      for (y <- 0 until gridSize; x <- 0 until gridSize) {
        sumGrid(y + 1)(x + 1) = grid(y)(x) + sumGrid(y + 1)(x) + sumGrid(y)(x + 1) - sumGrid(y)(x)
      }

      for (y <- 0 to gridSize - size; x <- 0 to gridSize - size) {
        val totalPower = sumGrid(y + size)(x + size) - sumGrid(y)(x + size) - sumGrid(y + size)(x) + sumGrid(y)(x)
        if (totalPower > maxPower) {
          maxPower = totalPower
          maxX = x + 1
          maxY = y + 1
          maxSize = size
        }
      }
    }

    println(s"$maxX,$maxY,$maxSize")
  }
}
