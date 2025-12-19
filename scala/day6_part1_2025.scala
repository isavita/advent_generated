
import scala.io.Source
import scala.math.BigInt
import java.io.File

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toVector
    if (lines.isEmpty) {
      println("Grand total: 0")
      return
    }

    val maxWidth = lines.map(_.length).max
    val isSep = Array.fill[Boolean](maxWidth)(true)

    for (x <- 0 until maxWidth) {
      var allSpace = true
      var i = 0
      while (i < lines.length && allSpace) {
        val line = lines(i)
        if (x < line.length && !Character.isWhitespace(line.charAt(x))) allSpace = false
        i += 1
      }
      isSep(x) = allSpace
    }

    var total = BigInt(0)
    var inBlock = false
    var start = 0

    for (x <- 0 until maxWidth) {
      if (!isSep(x)) {
        if (!inBlock) { inBlock = true; start = x }
      } else {
        if (inBlock) {
          total += processBlock(lines, start, x - 1)
          inBlock = false
        }
      }
    }
    if (inBlock) total += processBlock(lines, start, maxWidth - 1)

    println(s"Grand total: $total")
  }

  private def processBlock(lines: Vector[String], start: Int, end: Int): BigInt = {
    var op: String = ""
    val nums = scala.collection.mutable.ArrayBuffer.empty[BigInt]

    for (line <- lines) {
      if (start < line.length) {
        val seg = line.slice(start, math.min(end + 1, line.length)).trim
        if (seg.nonEmpty) {
          if (seg == "+" || seg == "*") op = seg
          else {
            try { nums += BigInt(seg) } catch { case _: NumberFormatException => }
          }
        }
      }
    }

    if (nums.isEmpty) BigInt(0)
    else if (op == "+") nums.foldLeft(BigInt(0))(_ + _)
    else if (op == "*") nums.foldLeft(BigInt(1))(_ * _)
    else if (nums.size == 1) nums.head
    else BigInt(0)
  }
}
