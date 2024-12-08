
import scala.io.Source

object XMASCounter extends App {

  def checkMAS(grid: Array[String], x: Int, y: Int, dx: Int, dy: Int): Boolean = {
    val word = "MAS"
    (0 until word.length).forall { i =>
      val newX = x + dx * i
      val newY = y + dy * i
      newX >= 0 && newY >= 0 && newX < grid.length && newY < grid(0).length && grid(newX)(newY) == word(i)
    } || (0 until word.length).forall { i =>
      val newX = x + dx * i
      val newY = y + dy * i
      newX >= 0 && newY >= 0 && newX < grid.length && newY < grid(0).length && grid(newX)(newY) == word(word.length - 1 - i)
    }
  }

  def checkXMAS(grid: Array[String], x: Int, y: Int): Boolean = {
    checkMAS(grid, x - 1, y - 1, 1, 1) && checkMAS(grid, x - 1, y + 1, 1, -1) ||
      checkMAS(grid, x + 1, y - 1, -1, 1) && checkMAS(grid, x + 1, y + 1, -1, -1)
  }

  def countXMASPatterns(grid: Array[String]): Int = {
    if (grid.length < 3 || grid(0).length < 3) 0
    else {
      (1 until grid.length - 1).flatMap(i =>
        (1 until grid(i).length - 1).map(j => (i, j))
      ).count { case (i, j) => grid(i)(j) == 'A' && checkXMAS(grid, i, j) }
    }
  }


  val grid = Source.fromFile("input.txt").getLines().filter(_.nonEmpty).toArray
  val count = countXMASPatterns(grid)
  println(s"X-MAS patterns appear $count times in the word search")
}
