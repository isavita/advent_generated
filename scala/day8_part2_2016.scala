
import scala.io.Source

val screenWidth = 50
val screenHeight = 6

def main(args: Array[String]): Unit = {
  val file = Source.fromFile("input.txt")
  val screen = Array.ofDim[Boolean](screenHeight, screenWidth)

  for (line <- file.getLines) {
    processInstruction(line, screen)
  }

  displayScreen(screen)
}

def displayScreen(screen: Array[Array[Boolean]]): Unit = {
  for (row <- screen) {
    for (pixel <- row) {
      if (pixel) {
        print("#")
      } else {
        print(".")
      }
    }
    println()
  }
}

def processInstruction(instruction: String, screen: Array[Array[Boolean]]): Unit = {
  val rectRegex = """rect (\d+)x(\d+)""".r
  val rotateRowRegex = """rotate row y=(\d+) by (\d+)""".r
  val rotateColumnRegex = """rotate column x=(\d+) by (\d+)""".r

  instruction match {
    case rectRegex(a, b) => rect(screen, a.toInt, b.toInt)
    case rotateRowRegex(a, b) => rotateRow(screen, a.toInt, b.toInt)
    case rotateColumnRegex(a, b) => rotateColumn(screen, a.toInt, b.toInt)
  }
}

def rect(screen: Array[Array[Boolean]], a: Int, b: Int): Unit = {
  for (y <- 0 until b) {
    for (x <- 0 until a) {
      screen(y)(x) = true
    }
  }
}

def rotateRow(screen: Array[Array[Boolean]], row: Int, shift: Int): Unit = {
  val temp = screen(row).map(x => false)
  for (i <- screen(row).indices) {
    temp((i + shift) % screenWidth) = screen(row)(i)
  }
  screen(row) = temp
}

def rotateColumn(screen: Array[Array[Boolean]], col: Int, shift: Int): Unit = {
  val temp = Array.ofDim[Boolean](screenHeight)
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
