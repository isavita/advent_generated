
import scala.io.Source

object TubeRouting {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toArray
    val (startX, startY) = findStartPosition(lines)
    val (letters, _) = followPath(lines, startX, startY)
    println(letters.mkString(""))
  }

  def findStartPosition(lines: Array[String]): (Int, Int) = {
    val startX = lines(0).indexOf('|')
    (startX, 0)
  }

  def followPath(lines: Array[String], startX: Int, startY: Int): (List[Char], (Int, Int)) = {
    var x = startX
    var y = startY
    var direction = (0, 1) // Start moving down
    var letters = List[Char]()

    while (y < lines.length && y >= 0 && x < lines(y).length && x >= 0) {
      val currentChar = lines(y)(x)

      currentChar match {
        case '|' | '-' =>
          // Continue in the same direction
        case '+' =>
          // Change direction
          direction = turn(lines, x, y, direction)
        case c if c.isLetter =>
          letters = letters :+ c
        case ' ' =>
          // If we hit a space, we should stop
          return (letters, (x, y))
      }

      // Move in the current direction
      x += direction._1
      y += direction._2
    }

    (letters, (x, y))
  }

  def turn(lines: Array[String], x: Int, y: Int, direction: (Int, Int)): (Int, Int) = {
    val (dx, dy) = direction
    val possibleTurns = List(
      (dx, dy) match {
        case (0, 1) => List((1, 0), (-1, 0)) // Down can turn right or left
        case (1, 0) => List((0, 1), (0, -1)) // Right can turn down or up
        case (0, -1) => List((1, 0), (-1, 0)) // Up can turn right or left
        case (-1, 0) => List((0, 1), (0, -1)) // Left can turn down or up
      }
    ).flatten

    for ((newDx, newDy) <- possibleTurns) {
      val newX = x + newDx
      val newY = y + newDy
      if (newY >= 0 && newY < lines.length && newX >= 0 && newX < lines(newY).length && 
          (lines(newY)(newX) == '|' || lines(newY)(newX) == '-' || lines(newY)(newX).isLetter || lines(newY)(newX) == '+')) {
        return (newDx, newDy)
      }
    }

    direction // Should not reach here
  }
}
