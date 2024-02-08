
import scala.io.Source

object Solution extends App {
  val rules = Source.fromFile("input.txt").getLines().map(line => {
    val parts = line.split(" => ")
    (parts(0), parts(1))
  }).toMap

  var grid = Array(
    ".#.",
    "..#",
    "###"
  )

  for (_ <- 0 until 5) {
    val (subSize, newSize) = if (grid.length % 2 == 0) {
      (2, grid.length / 2 * 3)
    } else {
      (3, grid.length / 3 * 4)
    }

    val newGrid = Array.fill(newSize)("")
    for (x <- 0 until newSize) {
      newGrid(x) = ""
    }

    for (y <- 0 until grid.length by subSize) {
      for (x <- 0 until grid.length by subSize) {
        var square = Array[String]()
        for (dy <- 0 until subSize) {
          square :+= grid(y + dy).substring(x, x + subSize)
        }
        val newSquare = enhance(square.mkString("/"), rules)
        for ((row, dy) <- newSquare.split("/").zipWithIndex) {
          newGrid(y / subSize * (subSize + 1) + dy) += row
        }
      }
    }
    grid = newGrid
  }

  var count = 0
  for (row <- grid) {
    for (pixel <- row) {
      if (pixel == '#') {
        count += 1
      }
    }
  }
  println(count)

  def enhance(input: String, rules: Map[String, String]): String = {
    var currentInput = input
    for (_ <- 0 until 4) {
      if (rules.contains(currentInput)) {
        return rules(currentInput)
      }
      currentInput = rotate(currentInput)
    }
    currentInput = flip(input)
    for (_ <- 0 until 4) {
      if (rules.contains(currentInput)) {
        return rules(currentInput)
      }
      currentInput = rotate(currentInput)
    }
    ""
  }

  def rotate(input: String): String = {
    val parts = input.split("/")
    val size = parts.length
    val newParts = Array.fill(size)("")

    for (x <- 0 until size) {
      var newRow = ""
      for (y <- size - 1 to 0 by -1) {
        newRow += parts(y)(x)
      }
      newParts(x) = newRow
    }
    newParts.mkString("/")
  }

  def flip(input: String): String = {
    input.split("/").map(reverse).mkString("/")
  }

  def reverse(input: String): String = {
    input.reverse
  }
}
