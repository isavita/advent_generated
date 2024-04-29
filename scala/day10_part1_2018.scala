import scala.io.Source
import scala.util.Try

case class Star(var x: Int, var y: Int, vX: Int, vY: Int, var next: Star = null)

object Main extends App {
  val input = Source.fromFile("input.txt").getLines().toList
  val head = new Star(0, 0, 0, 0)
  var tail = head
  val re = """position=<\s*(-?\d+),\s*(-?\d+)>\s*velocity=<\s*(-?\d+),\s*(-?\d+)>""".r
  for (line <- input) {
    line match {
      case re(x, y, vX, vY) =>
        val star = new Star(x.toInt, y.toInt, vX.toInt, vY.toInt)
        tail.next = star
        tail = star
    }
  }

  var smallestT = 0
  var smallestArea = Int.MaxValue
  for (t <- 1 to 100000) {
    var maxX = Int.MinValue
    var maxY = Int.MinValue
    var minX = Int.MaxValue
    var minY = Int.MaxValue

    var temp = head.next
    while (temp != null) {
      val x = temp.x + temp.vX * t
      if (x > maxX) maxX = x
      else if (x < minX) minX = x
      val y = temp.y + temp.vY * t
      if (y > maxY) maxY = y
      else if (y < minY) minY = y
      temp = temp.next
    }

    val lenX = maxX - minX + 1
    val lenY = maxY - minY + 1
    val area = lenX + lenY

    if (area < smallestArea) {
      smallestArea = area
      smallestT = t
    }
  }

  var t = smallestT

  var maxX = Int.MinValue
  var maxY = Int.MinValue
  var minX = Int.MaxValue
  var minY = Int.MaxValue

  var temp = head.next
  while (temp != null) {
    temp.x = temp.x + temp.vX * t
    if (temp.x > maxX) maxX = temp.x
    else if (temp.x < minX) minX = temp.x
    temp.y = temp.y + temp.vY * t
    if (temp.y > maxY) maxY = temp.y
    else if (temp.y < minY) minY = temp.y
    temp = temp.next
  }

  val mapper = Array.ofDim[Boolean](maxY - minY + 1, maxX - minX + 1)

  temp = head.next
  while (temp != null) {
    mapper(temp.y - minY)(temp.x - minX) = true
    temp = temp.next
  }

  for (i <- 0 until mapper.length) {
    for (j <- 0 until mapper(0).length) {
      if (mapper(i)(j)) print("#") else print(" ")
    }
    println()
  }
}