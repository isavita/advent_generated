
import scala.io.Source

object Main extends App {
  case class Point(x: Int, y: Int)

  val lines = Source.fromFile("input.txt").getLines().toList
  val wire1 = getPointsWithSteps(lines(0))
  val wire2 = getPointsWithSteps(lines(1))

  val minSteps = wire1.filter { case (p, steps1) => wire2.contains(p) }
    .map { case (p, steps1) => steps1 + wire2(p) }
    .min

  println(minSteps)

  def getPointsWithSteps(path: String): Map[Point, Int] = {
    var points = Map.empty[Point, Int]
    var current = Point(0, 0)
    var steps = 0

    path.split(",").foreach { move =>
      val dir = move(0)
      val dist = move.substring(1).toInt
      (1 to dist).foreach { _ =>
        steps += 1
        dir match {
          case 'U' => current = current.copy(y = current.y + 1)
          case 'D' => current = current.copy(y = current.y - 1)
          case 'L' => current = current.copy(x = current.x - 1)
          case 'R' => current = current.copy(x = current.x + 1)
        }
        if (!points.contains(current)) points += (current -> steps)
      }
    }

    points
  }
}
