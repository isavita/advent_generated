
import scala.io.Source

object Solution {
  case class Coord(var x: Int, var y: Int, var z: Int)
  case class Brick(mini: Coord, maxi: Coord, var basedOn: List[Brick] = Nil, var support: List[Brick] = Nil)

  def parseInput(input: List[String]): List[Brick] = {
    input.map { line =>
      val values = line.split("~")
      val miniValues = values(0).split(",").map(_.toInt)
      val maxiValues = values(1).split(",").map(_.toInt)
      Brick(Coord(miniValues(0), miniValues(1), miniValues(2)), Coord(maxiValues(0), maxiValues(1), maxiValues(2)))
    }
  }

  def settle(bricks: List[Brick]): Unit = {
    val sortedBricks = bricks.sortBy(_.maxi.z)

    for (i <- sortedBricks.indices) {
      var supportZ = 0
      var basedBricks: List[Brick] = Nil

      for (j <- (i - 1) to 0 by -1) {
        val isIntersectingX = math.max(sortedBricks(i).mini.x, sortedBricks(j).mini.x) <= math.min(sortedBricks(i).maxi.x, sortedBricks(j).maxi.x)
        val isIntersectingY = math.max(sortedBricks(i).mini.y, sortedBricks(j).mini.y) <= math.min(sortedBricks(i).maxi.y, sortedBricks(j).maxi.y)
        if (isIntersectingX && isIntersectingY) {
          if (sortedBricks(j).maxi.z == supportZ) {
            basedBricks ::= sortedBricks(j)
          } else if (sortedBricks(j).maxi.z > supportZ) {
            supportZ = sortedBricks(j).maxi.z
            basedBricks = List(sortedBricks(j))
          }
        }
      }

      sortedBricks(i).basedOn = basedBricks
      basedBricks.foreach(b => b.support ::= sortedBricks(i))

      val deltaZ = sortedBricks(i).maxi.z - sortedBricks(i).mini.z
      sortedBricks(i).mini.z = supportZ + 1
      sortedBricks(i).maxi.z = sortedBricks(i).mini.z + deltaZ
    }
  }

  def solve(input: List[String]): Int = {
    val bricks = parseInput(input)
    settle(bricks)

    bricks.count { brick =>
      brick.support.forall(_.basedOn.size >= 2)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    println(solve(input))
  }
}
