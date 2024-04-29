import scala.io.Source
import scala.collection.mutable

object Main {
  val memo = mutable.Map[String, String]()

  def main(args: Array[String]): Unit = {
    val rules = mutable.Map[String, String]()
    for (line <- Source.fromFile("input.txt").getLines) {
      val parts = line.split(" => ")
      rules(parts(0)) = parts(1)
    }

    var grid = Array(".#.", "..#", "###")

    for (_ <- 0 until 18) {
      var newSize = 0
      var subSize = 0

      if (grid.length % 2 == 0) {
        subSize = 2
        newSize = grid.length / 2 * 3
      } else {
        subSize = 3
        newSize = grid.length / 3 * 4
      }

      val newGrid = new Array[String](newSize)
      for (x <- 0 until newSize) {
        newGrid(x) = ""
      }

      for (y <- 0 until grid.length by subSize) {
        for (x <- 0 until grid.length by subSize) {
          val square = new Array[String](subSize)
          for (dy <- 0 until subSize) {
            square(dy) = grid(y + dy).substring(x, x + subSize)
          }
          var newSquare = enhance(square.mkString("/"), rules)
          for (dy <- 0 until newSquare.split("/").length) {
            newGrid(y / subSize * (subSize + 1) + dy) += newSquare.split("/")(dy)
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
  }

  def enhance(input: String, rules: mutable.Map[String, String]): String = {
    if (memo.contains(input)) {
      memo(input)
    } else {
      var original = input
      var tempInput = input
      for (_ <- 0 until 4) {
        if (rules.contains(tempInput)) {
          memo(original) = rules(tempInput)
          return rules(tempInput)
        }
        tempInput = rotate(tempInput)
      }
      tempInput = flip(tempInput)
      for (_ <- 0 until 4) {
        if (rules.contains(tempInput)) {
          memo(original) = rules(tempInput)
          return rules(tempInput)
        }
        tempInput = rotate(tempInput)
      }
      ""
    }
  }

  def rotate(input: String): String = {
    val parts = input.split("/")
    val size = parts.length
    val newParts = new Array[String](size)
    for (x <- 0 until size) {
      var newRow = ""
      for (y <- size - 1 to 0 by -1) {
        newRow += parts(y).charAt(x).toString
      }
      newParts(x) = newRow
    }
    newParts.mkString("/")
  }

  def flip(input: String): String = {
    val parts = input.split("/")
    for (i <- 0 until parts.length) {
      parts(i) = reverse(parts(i))
    }
    parts.mkString("/")
  }

  def reverse(input: String): String = {
    val runes = input.toArray
    var i = 0
    var j = runes.length - 1
    while (i < j) {
      val temp = runes(i)
      runes(i) = runes(j)
      runes(j) = temp
      i += 1
      j -= 1
    }
    new String(runes)
  }
}