import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    val wire1 = input(0).split(",")
    val wire2 = input(1).split(",")

    val path1 = getPath(wire1)
    val path2 = getPath(wire2)

    val intersections = path1.keys.toSet.intersect(path2.keys.toSet)
    val minDistance = intersections.map(p => p._1.abs + p._2.abs).min

    println(minDistance)
  }

  def getPath(wire: Array[String]): Map[(Int, Int), Int] = {
    var x = 0
    var y = 0
    var steps = 0
    var path = Map[(Int, Int), Int]()

    for (move <- wire) {
      val dir = move.head
      val dist = move.tail.toInt

      for (_ <- 1 to dist) {
        steps += 1
        dir match {
          case 'U' => y += 1
          case 'D' => y -= 1
          case 'L' => x -= 1
          case 'R' => x += 1
        }
        path += ((x, y) -> steps)
      }
    }

    path
  }
}