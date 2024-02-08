
import scala.io.Source

object Solution {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input.txt")
    val lines = file.getLines().toList
    file.close()

    val count = lines.map(_.split(",")).count(pair => {
      val left = parseRange(pair(0))
      val right = parseRange(pair(1))
      left(0) <= right(1) && left(1) >= right(0)
    })

    println(count)
  }

  def parseRange(s: String): Array[Int] = {
    val split = s.split("-")
    Array(split(0).toInt, split(1).toInt)
  }
}
