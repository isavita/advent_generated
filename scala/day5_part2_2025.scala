
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val ranges = Source.fromFile("input.txt")
      .getLines()
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap { line =>
        line.split("-") match {
          case Array(a, b) =>
            try {
              val x = a.toLong
              val y = b.toLong
              if (x <= y) Some((x, y)) else Some((y, x))
            } catch { case _: Throwable => None }
          case _ => None
        }
      }
      .toSeq

    if (ranges.isEmpty) {
      println("Total fresh IDs: 0")
      return
    }

    val sorted = ranges.sortBy(_._1)
    var total: Long = 0
    var curMin = sorted.head._1
    var curMax = sorted.head._2

    for ((min, max) <- sorted.tail) {
      if (min <= curMax) {
        if (max > curMax) curMax = max
      } else {
        total += curMax - curMin + 1
        curMin = min
        curMax = max
      }
    }
    total += curMax - curMin + 1

    println(s"Total fresh IDs: $total")
  }
}
