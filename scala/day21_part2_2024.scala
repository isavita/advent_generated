
object Solution {
  def findPosition(mat: Array[String], ch: Char): (Int, Int) = {
    for (i <- mat.indices; j <- mat(i).indices if mat(i)(j) == ch) return (i, j)
    (-1, -1)
  }

  def ok(mat: Array[String], st: (Int, Int), seq: String): Boolean = {
    var (currI, currJ) = st
    for (ch <- seq) {
      if (!(0 <= currI && currI < mat.length && 0 <= currJ && currJ < mat(currI).length) || mat(currI)(currJ) == ' ')
        return false
      ch match {
        case '^' => currI -= 1
        case 'v' => currI += 1
        case '<' => currJ -= 1
        case '>' => currJ += 1
        case _   =>
      }
    }
    true
  }

  def generateMoves(position: (Int, Int), objective: Char, pad: Array[String]): String = {
    val (objPosI, objPosJ) = findPosition(pad, objective)
    val (posI, posJ) = position

    val result1 = new StringBuilder()
    if (posJ > objPosJ) result1 ++= "<" * (posJ - objPosJ)
    if (posI > objPosI) result1 ++= "^" * (posI - objPosI)
    if (posI < objPosI) result1 ++= "v" * (objPosI - posI)
    if (posJ < objPosJ) result1 ++= ">" * (objPosJ - posJ)

    if (ok(pad, position, result1.toString)) {
      result1.toString()
    } else {
      val result2 = new StringBuilder()
      if (posJ < objPosJ) result2 ++= ">" * (objPosJ - posJ)
      if (posI > objPosI) result2 ++= "^" * (posI - objPosI)
      if (posI < objPosI) result2 ++= "v" * (objPosI - posI)
      if (posJ > objPosJ) result2 ++= "<" * (posJ - objPosJ)
      result2.toString()
    }
  }

  def solve(code: String, robots: Int, keyPad: Array[String], robotPad: Array[String],
            maxRobots: Int, memo: collection.mutable.Map[(String, Int, Int), Long]): Long = {
    val key = (code, robots, maxRobots)
    if (memo.contains(key)) return memo(key)

    if (robots <= 0) return code.length

    var ret = 0L
    var (posI, posJ) = (3, 2)
    if (robots != maxRobots) posI = 0

    for (ch <- code) {
      val moves =
        if (robots == maxRobots) {
          generateMoves((posI, posJ), ch, keyPad)
        } else {
          generateMoves((posI, posJ), ch, robotPad)
        }
      val (newPosI, newPosJ) =
        if (robots == maxRobots) findPosition(keyPad, ch)
        else findPosition(robotPad, ch)

      ret += solve(moves + "A", robots - 1, keyPad, robotPad, maxRobots, memo)
      posI = newPosI
      posJ = newPosJ
    }

    memo(key) = ret
    ret
  }

  def main(args: Array[String]): Unit = {
    val maxRobots = 26
    val keyPad = Array(
      "789",
      "456",
      "123",
      " 0A"
    )
    val robotPad = Array(
      " ^A",
      "<v>"
    )

    val lines = scala.io.Source.fromFile("input.txt").getLines().toList
    var ret = 0L

    for (code <- lines) {
      val trimmedCode = code.trim()
      if (trimmedCode.nonEmpty) {
        val numericPart = trimmedCode.filter(_.isDigit).mkString("").toLongOption.getOrElse(0L)
        ret += solve(trimmedCode, maxRobots, keyPad, robotPad, maxRobots, collection.mutable.Map.empty) * numericPart
      }
    }
    println(ret)
  }
}
