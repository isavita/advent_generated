
import scala.io.Source

object WordSearch extends App {

  case class Direction(dx: Int, dy: Int)

  val allDirections = List(
    Direction(0, 1),   // right
    Direction(1, 0),   // down
    Direction(1, 1),   // diagonal down-right
    Direction(-1, 1),  // diagonal up-right
    Direction(0, -1),  // left
    Direction(-1, 0),  // up
    Direction(-1, -1), // diagonal up-left
    Direction(1, -1)   // diagonal down-left
  )

  def checkWord(grid: Array[String], word: String, x: Int, y: Int, d: Direction): Boolean = {
    if (x < 0 || y < 0 || x >= grid.length || y >= grid(0).length) false
    else {
      var valid = true
      for (i <- 0 until word.length) {
        val newX = x + d.dx * i
        val newY = y + d.dy * i
        if (newX < 0 || newY < 0 || newX >= grid.length || newY >= grid(0).length || grid(newX)(newY) != word(i)) {
          valid = false
          
        }
      }
      valid
    }
  }

  def countOccurrences(grid: Array[String], word: String): Int = {
    var count = 0
    for {
      i <- grid.indices
      j <- grid(0).indices
      d <- allDirections
      if checkWord(grid, word, i, j, d)
    } count += 1
    count
  }

  val source = Source.fromFile("input.txt")
  val lines = source.getLines().filter(_.nonEmpty).toArray
  source.close()

  val count = countOccurrences(lines, "XMAS")
  println(s"XMAS appears $count times in the word search")
}
