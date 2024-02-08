import scala.io.Source

object Solution {
  def main(args: Array[String]): Unit = {
    val cubes = scala.collection.mutable.Set[(Int, Int, Int)]()
    val neighbors = List((-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1))
    var min = (Int.MaxValue, Int.MaxValue, Int.MaxValue)
    var max = (Int.MinValue, Int.MinValue, Int.MinValue)

    val lines = Source.fromFile("input.txt").getLines()
    for (line <- lines) {
      if (line.trim.nonEmpty) {
        val Array(x, y, z) = line.split(",").map(_.toInt)
        cubes.add((x, y, z))
        min = (Math.min(min._1, x), Math.min(min._2, y), Math.min(min._3, z))
        max = (Math.max(max._1, x), Math.max(max._2, y), Math.max(max._3, z))
      }
    }
    min = (min._1 - 1, min._2 - 1, min._3 - 1)
    max = (max._1 + 1, max._2 + 1, max._3 + 1)

    var faces = 0
    var q = List(min)
    var seen = Set(min)
    while (q.nonEmpty) {
      val curr = q.head
      q = q.tail
      for (delta <- neighbors) {
        val next = (curr._1 + delta._1, curr._2 + delta._2, curr._3 + delta._3)
        if (next._1 < min._1 || next._2 < min._2 || next._3 < min._3 ||
          next._1 > max._1 || next._2 > max._2 || next._3 > max._3) {
          // do nothing
        } else if (cubes.contains(next)) {
          faces += 1
        } else if (!seen.contains(next)) {
          seen += next
          q = q :+ next
        }
      }
    }
    println(faces)
  }
}