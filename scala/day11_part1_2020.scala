import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val seatingArea = Source.fromFile("input.txt").getLines().map(_.toArray).toArray
    var stabilized = false
    var newSeatingArea = seatingArea
    while (!stabilized) {
      val (newArea, stab) = simulateSeating(newSeatingArea)
      newSeatingArea = newArea
      stabilized = stab
    }
    println(countOccupiedSeats(newSeatingArea))
  }

  def simulateSeating(seatingArea: Array[Array[Char]]): (Array[Array[Char]], Boolean) = {
    val rows = seatingArea.length
    val cols = seatingArea(0).length
    val newSeatingArea = Array.ofDim[Char](rows, cols)
    for (i <- 0 until rows; j <- 0 until cols) {
      newSeatingArea(i)(j) = seatingArea(i)(j)
    }
    var stabilized = true

    for (i <- 0 until rows; j <- 0 until cols) {
      seatingArea(i)(j) match {
        case 'L' =>
          if (countAdjacentOccupied(seatingArea, i, j) == 0) {
            newSeatingArea(i)(j) = '#'
            stabilized = false
          } else {
            newSeatingArea(i)(j) = 'L'
          }
        case '#' =>
          if (countAdjacentOccupied(seatingArea, i, j) >= 4) {
            newSeatingArea(i)(j) = 'L'
            stabilized = false
          } else {
            newSeatingArea(i)(j) = '#'
          }
        case '.' =>
          newSeatingArea(i)(j) = '.'
      }
    }

    (newSeatingArea, stabilized)
  }

  def countAdjacentOccupied(seatingArea: Array[Array[Char]], row: Int, col: Int): Int = {
    var count = 0
    for (i <- row - 1 to row + 1; j <- col - 1 to col + 1) {
      if (i != row || j != col) {
        if (i >= 0 && i < seatingArea.length && j >= 0 && j < seatingArea(0).length) {
          if (seatingArea(i)(j) == '#') {
            count += 1
          }
        }
      }
    }
    count
  }

  def countOccupiedSeats(seatingArea: Array[Array[Char]]): Int = {
    var count = 0
    for (row <- seatingArea; seat <- row) {
      if (seat == '#') {
        count += 1
      }
    }
    count
  }
}