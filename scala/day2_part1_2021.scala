import scala.io.Source

object Dive {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    val (horizontalPosition, depth) = input.foldLeft((0, 0)) { case ((h, d), command) =>
      val split = command.split(" ")
      val value = split(1).toInt
      split(0) match {
        case "forward" => (h + value, d)
        case "down" => (h, d + value)
        case "up" => (h, d - value)
      }
    }
    println(horizontalPosition * depth)
  }
}