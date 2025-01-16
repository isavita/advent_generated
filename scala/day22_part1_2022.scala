
import scala.io.Source
import scala.util.Using

object Solution {

  case class P(x: Int, y: Int)

  object Dir extends Enumeration {
    type Dir = Value
    val N, E, S, W = Value
  }

  import Dir._

  implicit class DirOps(dir: Dir) {
    def rotate(direction: Char): Dir = direction match {
      case 'R' => Dir((dir.id + 1) % 4)
      case 'L' => Dir((dir.id - 1 + 4) % 4)
      case _ => dir
    }

    def points: Int = (dir.id + 3) % 4
  }

  case class Movement(steps: Int, rotate: Char)

  val Dirs = Array(P(-1, 0), P(0, 1), P(1, 0), P(0, -1))

  case class Human(var curr: P, var facing: Dir) {
    def walk(map: Map[P, Boolean]): Boolean = {
      val dir = Dirs(facing.id)
      var next = P(curr.x + dir.x, curr.y + dir.y)

      if (map.contains(next)) {
        if (map(next)) {
          return false
        }
        curr = next
        return true
      }

      val oppDir = P(-dir.x, -dir.y)
      while (true) {
        val lookAhead = P(next.x + oppDir.x, next.y + oppDir.y)
        if (!map.contains(lookAhead)) {
          if (map(next)) {
            return false
          }
          curr = next
          return true
        }
        next = lookAhead
      }
      false
    }
  }

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("input.txt")) { source =>
      val lines = source.getLines().toSeq
      val (map, movements, size) = parse(lines)

      val human = Human(P(0, size), E)

      movements.foreach { mov =>
        human.facing = human.facing.rotate(mov.rotate)
        (0 until mov.steps).takeWhile(_ => human.walk(map))
      }

      println(1000 * (human.curr.x + 1) + 4 * (human.curr.y + 1) + human.facing.points)
    }
  }

  def parse(lines: Seq[String]): (Map[P, Boolean], Seq[Movement], Int) = {
    var size = 0
    var map = Map.empty[P, Boolean]
    var r = 0
    var emptyLineFound = false

    for (line <- lines if !emptyLineFound) {
      if (line.isEmpty) {
        emptyLineFound = true
      } else {
        if (r == 0) {
          size = line.length / 3
        }
        line.zipWithIndex.foreach {
          case (' ', _) =>
          case ('#', c) => map += (P(r, c) -> true)
          case ('.', c) => map += (P(r, c) -> false)
          case _ =>
        }
        r += 1
      }
    }

    val path = lines.dropWhile(!_.isEmpty).drop(1).headOption.getOrElse("")
    val movements = parsePath(path)
    (map, movements, size)
  }

  def parsePath(path: String): Seq[Movement] = {
    var movements = Seq.empty[Movement]
    var acc = 0
    for (char <- path) {
      char match {
        case 'R' =>
          movements = movements :+ Movement(acc, ' ')
          acc = 0
          movements = movements :+ Movement(0, 'R')
        case 'L' =>
          movements = movements :+ Movement(acc, ' ')
          acc = 0
          movements = movements :+ Movement(0, 'L')
        case digit if digit.isDigit =>
          acc = 10 * acc + (digit - '0')
        case _ =>
      }
    }
    movements :+ Movement(acc, ' ')
  }
}
