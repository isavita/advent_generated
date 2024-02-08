
import scala.io.Source

object Solution {
  val screenWidth = 50
  val screenHeight = 6

  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input.txt")
    val screen = Array.ofDim[Boolean](screenHeight, screenWidth)

    for (line <- file.getLines) {
      processInstruction(line, screen)
    }

    println(countLitPixels(screen))
    file.close()
  }

  def processInstruction(instruction: String, screen: Array[Array[Boolean]]): Unit = {
    val rectRegex = "rect (\\d+)x(\\d+)".r
    val rotateRowRegex = "rotate row y=(\\d+) by (\\d+)".r
    val rotateColumnRegex = "rotate column x=(\\d+) by (\\d+)".r

    instruction match {
      case rectRegex(a, b) => rect(screen, a.toInt, b.toInt)
      case rotateRowRegex(row, shift) => rotateRow(screen, row.toInt, shift.toInt)
      case rotateColumnRegex(col, shift) => rotateColumn(screen, col.toInt, shift.toInt)
    }
  }

  def rect(screen: Array[Array[Boolean]], a: Int, b: Int): Unit = {
    for (y <- 0 until b; x <- 0 until a) {
      screen(y)(x) = true
    }
  }

  def rotateRow(screen: Array[Array[Boolean]], row: Int, shift: Int): Unit = {
    val temp = Array.fill(screenWidth)(false)
    for (i <- screen(row).indices) {
      temp((i + shift) % screenWidth) = screen(row)(i)
    }
    screen(row) = temp
  }

  def rotateColumn(screen: Array[Array[Boolean]], col: Int, shift: Int): Unit = {
    val temp = Array.fill(screenHeight)(false)
    for (i <- screen.indices) {
      temp((i + shift) % screenHeight) = screen(i)(col)
    }
    for (i <- screen.indices) {
      screen(i)(col) = temp(i)
    }
  }

  def countLitPixels(screen: Array[Array[Boolean]]): Int = {
    screen.map(_.count(_ == true)).sum
  }
}
