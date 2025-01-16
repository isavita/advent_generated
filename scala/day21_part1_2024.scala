
import scala.io.Source

case class Position(i: Int, j: Int)

object Solution {
  def findPosition(mat: Array[String], ch: Char): Position = {
    for (i <- mat.indices; j <- mat(i).indices) {
      if (mat(i)(j) == ch) return Position(i, j)
    }
    Position(-1, -1)
  }

  def ok(mat: Array[String], st: Position, seq: String): Boolean = {
    var curr = st
    for (ch <- seq) {
      if (mat(curr.i)(curr.j) == ' ') return false
      ch match {
        case '^' => curr = curr.copy(i = curr.i - 1)
        case 'v' => curr = curr.copy(i = curr.i + 1)
        case '<' => curr = curr.copy(j = curr.j - 1)
        case '>' => curr = curr.copy(j = curr.j + 1)
      }
      if (curr.i < 0 || curr.i >= mat.length || curr.j < 0 || curr.j >= mat(0).length) return false
    }
    true
  }

  def generateMoves(position: Position, objective: Char, pad: Array[String]): String = {
    val objPos = findPosition(pad, objective)
    val sb = new StringBuilder

    if (position.j > objPos.j) sb.append("<" * (position.j - objPos.j))
    if (position.i > objPos.i) sb.append("^" * (position.i - objPos.i))
    if (position.i < objPos.i) sb.append("v" * (objPos.i - position.i))
    if (position.j < objPos.j) sb.append(">" * (objPos.j - position.j))

    if (!ok(pad, position, sb.toString)) {
      sb.clear()
      if (position.j < objPos.j) sb.append(">" * (objPos.j - position.j))
      if (position.i > objPos.i) sb.append("^" * (position.i - objPos.i))
      if (position.i < objPos.i) sb.append("v" * (objPos.i - position.i))
      if (position.j > objPos.j) sb.append("<" * (position.j - objPos.j))
    }

    sb.toString
  }

  def solve(code: String, robots: Int, keyPad: Array[String], robotPad: Array[String], maxRobots: Int): Int = {
    if (robots <= 0) return code.length

    var ret = 0
    var (posi, posj) = (3, 2)
    if (robots != maxRobots) posi = 0

    for (ch <- code) {
      val moves = if (robots == maxRobots) {
        generateMoves(Position(posi, posj), ch, keyPad)
      } else {
        generateMoves(Position(posi, posj), ch, robotPad)
      }
      val pos = if (robots == maxRobots) findPosition(keyPad, ch) else findPosition(robotPad, ch)
      posi = pos.i
      posj = pos.j
      ret += solve(moves + "A", robots - 1, keyPad, robotPad, maxRobots)
    }
    ret
  }

  def main(args: Array[String]): Unit = {
    val content = Source.fromFile("input.txt").getLines().toList
    val maxRobots = 3
    val keyPad = Array("789", "456", "123", " 0A")
    val robotPad = Array(" ^A", "<v>")

    var ret = 0
    for (code <- content) {
      val numericPart = code.filter(_.isDigit).toInt
      val sv = solve(code, maxRobots, keyPad, robotPad, maxRobots)
      ret += sv * numericPart
    }

    println(ret)
  }
}
