
import scala.io.Source
import scala.util.Using
import scala.util.Try

case class Machine(ax: Int, ay: Int, bx: Int, by: Int, px: Int, py: Int)

object Solution {
  def main(args: Array[String]): Unit = {
    val machines = readInput("input.txt")
    val results = machines.flatMap(solveMachine)

    if (results.isEmpty) {
      println("0 0")
    } else {
      val count = results.size
      val sum = results.sum
      println(s"$count $sum")
    }
  }

  def readInput(filename: String): List[Machine] = {
    Using(Source.fromFile(filename)) { source =>
      source.getLines()
        .foldLeft((List.empty[Machine], List.empty[String])) {
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
    }.get
  }

  def parseMachine(lines: List[String]): Machine = {
    var ax, ay, bx, by, px, py = 0
    lines.foreach { line =>
      val l = line.replaceAll("Button A:", "A:").replaceAll("Button B:", "B:").replaceAll("Prize:", "P:")
      if (l.startsWith("A:")) {
        val (x, y) = parseLine(l.substring(2))
        ax = x
        ay = y
      } else if (l.startsWith("B:")) {
        val (x, y) = parseLine(l.substring(2))
        bx = x
        by = y
      } else if (l.startsWith("P:")) {
        val (x, y) = parsePrize(l.substring(2))
        px = x
        py = y
      }
    }
    Machine(ax, ay, bx, by, px, py)
  }

  def parseLine(s: String): (Int, Int) = {
    val parts = s.trim.split(",")
    val x = parseVal(parts(0).trim)
    val y = parseVal(parts(1).trim)
    (x, y)
  }

  def parsePrize(s: String): (Int, Int) = {
    val parts = s.trim.split(",")
    val x = parseValPrize(parts(0).trim)
    val y = parseValPrize(parts(1).trim)
    (x, y)
  }

  def parseVal(s: String): Int = {
    val trimmed = s.trim.replaceAll("X\\+|Y\\+|X=|Y=", "")
    Try(trimmed.toInt).getOrElse(0)
  }

  def parseValPrize(s: String): Int = {
    val trimmed = s.trim.replaceAll("X=|Y=", "")
    Try(trimmed.toInt).getOrElse(0)
  }

  def solveMachine(m: Machine): Option[Int] = {
    (0 to 100).flatMap { aCount =>
      (0 to 100).map { bCount =>
        val x = m.ax * aCount + m.bx * bCount
        val y = m.ay * aCount + m.by * bCount
        if (x == m.px && y == m.py) Some(aCount * 3 + bCount) else None
      }.collect{case Some(cost) => cost}
    }.minOption
  }
}
