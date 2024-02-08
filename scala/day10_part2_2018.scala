
import scala.io.Source
import scala.util.matching.Regex

object Solution {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    val head = new Star(0, 0, 0, 0, null)
    var tail = head
    val re = """position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>""".r
    for (line <- input) {
      re.findFirstMatchIn(line) match {
        case Some(m) =>
          val star = new Star(m.group(1).toInt, m.group(2).toInt, m.group(3).toInt, m.group(4).toInt, null)
          tail.next = star
          tail = star
        case None =>
      }
    }

    var smallestT = 0
    var smallestArea = Int.MaxValue
    for (t <- 1 until 100000) {
      var maxX = 0
      var maxY = 0
      var minX = 0
      var minY = 0

      var temp = head.next
      while (temp.next != null) {
        val x = temp.x + temp.vX * t
        if (maxX < x) maxX = x else if (minX > x) minX = x
        val y = temp.y + temp.vY * t
        if (maxY < y) maxY = y else if (minY > y) minY = y
        temp = temp.next
      }

      val lenX = maxX - minX + 1
      val lenY = maxY - minY + 1
      val area = lenX + lenY

      if (smallestArea > area) {
        smallestArea = area
        smallestT = t
      }
    }

    println(smallestT)

    var t = smallestT
    var maxX = 0
    var maxY = 0
    var minX = 0
    var minY = 0

    var temp = head.next
    while (temp.next != null) {
      temp.x = temp.x + temp.vX * t
      if (maxX < temp.x) maxX = temp.x else if (minX > temp.x) minX = temp.x
      temp.y = temp.y + temp.vY * t
      if (maxY < temp.y) maxY = temp.y else if (minY > temp.y) minY = temp.y
      temp = temp.next
    }

    val mapper = Array.ofDim[Boolean](maxY - minY + 1, maxX - minX + 1)

    for (i <- mapper.indices) {
      mapper(i) = Array.fill(maxX - minX + 1)(false)
    }

    temp = head.next
    while (temp.next != null) {
      mapper(temp.y)(temp.x) = true
      temp = temp.next
    }
  }

  class Star(var x: Int, var y: Int, var vX: Int, var vY: Int, var next: Star)
}
