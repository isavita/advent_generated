object Solution extends App {
  val source = io.Source.fromFile("input.txt")
  val lines = try source.getLines.toList finally source.close()

  val gridSize = 1000
  val grid = Array.ofDim[Int](gridSize, gridSize)

  lines.foreach(instruction => processInstruction(instruction, grid))

  println(totalBrightness(grid))

  def processInstruction(instruction: String, grid: Array[Array[Int]]): Unit = {
    val parts = instruction.split(" ")
    val Array(startX, startY) = parts(parts.length - 3).split(",").map(_.toInt)
    val Array(endX, endY) = parts(parts.length - 1).split(",").map(_.toInt)

    for {
      x <- startX to endX
      y <- startY to endY
    } {
      instruction match {
        case i if i.startsWith("turn on") => grid(x)(y) += 1
        case i if i.startsWith("turn off") => if (grid(x)(y) > 0) grid(x)(y) -= 1
        case i if i.startsWith("toggle") => grid(x)(y) += 2
      }
    }
  }

  def totalBrightness(grid: Array[Array[Int]]): Int = grid.flatten.sum
}