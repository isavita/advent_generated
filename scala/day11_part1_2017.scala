import scala.io.Source

object HexEd {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().next().split(",").toList
    val (x, y, z) = input.foldLeft((0, 0, 0))((pos, dir) => {
      dir match {
        case "n" => (pos._1, pos._2 + 1, pos._3 - 1)
        case "ne" => (pos._1 + 1, pos._2, pos._3 - 1)
        case "se" => (pos._1 + 1, pos._2 - 1, pos._3)
        case "s" => (pos._1, pos._2 - 1, pos._3 + 1)
        case "sw" => (pos._1 - 1, pos._2, pos._3 + 1)
        case "nw" => (pos._1 - 1, pos._2 + 1, pos._3)
      }
    })
    val steps = List(x, y, z).map(Math.abs).max
    println(steps)
  }
}