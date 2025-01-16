
import java.io.File
import scala.io.Source

object Solution {
  case class Machine(ax: Long, ay: Long, bx: Long, by: Long, px: Long, py: Long)

  def main(args: Array[String]): Unit = {
    val offset = 10000000000000L
    val machines = readInput("input.txt").map(m => m.copy(px = m.px + offset, py = m.py + offset))
    val results = machines.flatMap(solveMachine)
    if (results.isEmpty) {
      println("0 0")
    } else {
      println(s"${results.size} ${results.sum}")
    }
  }

  def readInput(filename: String): List[Machine] = {
    val lines = Source.fromFile(new File(filename)).getLines().toList
    lines.foldLeft((List.empty[Machine], List.empty[String])) {
      case ((machines, currentLines), line) =>
        val trimmedLine = line.trim
        if (trimmedLine.isEmpty) {
          if (currentLines.nonEmpty) {
            (machines :+ parseMachine(currentLines), List.empty[String])
          } else {
            (machines, currentLines)
          }
        } else {
          (machines, currentLines :+ trimmedLine)
        }
    } match {
      case (machines, currentLines) =>
        if (currentLines.nonEmpty) machines :+ parseMachine(currentLines) else machines
    }
  }

  def parseMachine(lines: List[String]): Machine = {
    var ax, ay, bx, by, px, py = 0L
    lines.foreach { line =>
      val l = line.replaceAll("Button A:", "A:").replaceAll("Button B:", "B:").replaceAll("Prize:", "P:")
      if (l.startsWith("A:")) {
        val (x, y) = parseLine(l.substring(2))
        ax = x; ay = y
      } else if (l.startsWith("B:")) {
        val (x, y) = parseLine(l.substring(2))
        bx = x; by = y
      } else if (l.startsWith("P:")) {
        val (x, y) = parsePrize(l.substring(2))
        px = x; py = y
      }
    }
    Machine(ax, ay, bx, by, px, py)
  }

  def parseLine(s: String): (Long, Long) = {
    val parts = s.trim.split(",")
    (parseVal(parts(0)), parseVal(parts(1)))
  }

  def parsePrize(s: String): (Long, Long) = {
    val parts = s.trim.split(",")
    (parseValPrize(parts(0)), parseValPrize(parts(1)))
  }

  def parseVal(s: String): Long = {
    val trimmed = s.trim.replaceAll("X\\+|Y\\+|X=|Y=", "")
    trimmed.toLong
  }

  def parseValPrize(s: String): Long = {
    val trimmed = s.trim.replaceAll("X=|Y=", "")
    trimmed.toLong
  }

  def solveMachine(m: Machine): Option[Long] = {
    val D = m.ax * m.by - m.ay * m.bx
    if (D == 0) return None
    val numA = m.px * m.by - m.py * m.bx
    val numB = -m.px * m.ay + m.py * m.ax
    if (numA % D != 0 || numB % D != 0) return None
    val a = numA / D
    val b = numB / D
    if (a < 0 || b < 0) return None
    Some(3 * a + b)
  }
}
