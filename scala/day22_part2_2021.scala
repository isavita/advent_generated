
import scala.io.Source

object ReactorReboot {

  case class Cube(isOn: Boolean, x1: Int, x2: Int, y1: Int, y2: Int, z1: Int, z2: Int)

  def parseInput(inputStr: String): List[Cube] = {
    inputStr.split('\n').toList.map { line =>
      val parts = line.split(' ')
      val coords = parts(1).replace("x=", "").replace("y=", "").replace("z=", "").replace("..", ",").split(',').map(_.toInt)
      Cube(parts(0) == "on", coords(0), coords(1), coords(2), coords(3), coords(4), coords(5))
    }
  }

  def getIntersection(c1: Cube, c2: Cube): (Option[Cube], Boolean) = {
    val (x1, x2) = (math.max(c1.x1, c2.x1), math.min(c1.x2, c2.x2))
    val (y1, y2) = (math.max(c1.y1, c2.y1), math.min(c1.y2, c2.y2))
    val (z1, z2) = (math.max(c1.z1, c2.z1), math.min(c1.z2, c2.z2))

    if (x1 > x2 || y1 > y2 || z1 > z2) (None, false)
    else {
      val intersectionState = if (c1.isOn != c2.isOn) c2.isOn else !c1.isOn
      (Some(Cube(intersectionState, x1, x2, y1, y2, z1, z2)), true)
    }
  }

  def volume(c: Cube): Long =
    (c.x2 - c.x1 + 1).toLong * (c.y2 - c.y1 + 1) * (c.z2 - c.z1 + 1) * (if (c.isOn) 1 else -1)

  def solve(inputStr: String): Long = {
    val cubes = parseInput(inputStr)
    var finalList: List[Cube] = Nil

    for (c <- cubes) {
      val toAdd = finalList.flatMap { finalCube =>
        getIntersection(finalCube, c) match {
          case (Some(intersection), true) => List(intersection)
          case _ => Nil
        }
      }
      if (c.isOn) finalList = c :: (finalList ++ toAdd)
      else finalList = finalList ++ toAdd

    }
    finalList.map(volume).sum
  }

  def main(args: Array[String]): Unit = {
    val inputStr = Source.fromFile("input.txt").getLines().mkString("\n")
    val result = solve(inputStr)
    println(result)
  }
}
