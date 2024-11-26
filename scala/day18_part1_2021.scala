
import scala.io.Source

sealed trait SnailNumber
case class Pair(left: SnailNumber, right: SnailNumber) extends SnailNumber
case class Regular(value: Int) extends SnailNumber

object SnailMath {
  def parse(s: String): SnailNumber = {
    def parseRec(s: String): (SnailNumber, String) = {
      if (s.head == '[') {
        val (left, remaining1) = parseRec(s.tail)
        val (right, remaining2) = parseRec(remaining1.tail)
        (Pair(left, right), remaining2.tail)
      } else {
        val num = s.takeWhile(_.isDigit).toInt
        (Regular(num), s.dropWhile(_.isDigit))
      }
    }
    parseRec(s)._1
  }

  def add(a: SnailNumber, b: SnailNumber): SnailNumber = {
    def reduce(n: SnailNumber): SnailNumber = {
      def explode(n: SnailNumber, depth: Int): Option[(SnailNumber, Option[Int], Option[Int])] = n match {
        case Pair(Regular(x), Regular(y)) if depth >= 4 => Some((Regular(0), Some(x), Some(y)))
        case Pair(left, right) =>
          explode(left, depth + 1) match {
            case Some((newLeft, leftAdd, rightAdd)) =>
              Some((Pair(newLeft, addToLeftmost(right, rightAdd)), leftAdd, None))
            case None =>
              explode(right, depth + 1) match {
                case Some((newRight, leftAdd, rightAdd)) =>
                  Some((Pair(addToRightmost(left, leftAdd), newRight), None, rightAdd))
                case None => None
              }
          }
        case _ => None
      }

      def split(n: SnailNumber): Option[SnailNumber] = n match {
        case Regular(x) if x >= 10 =>
          Some(Pair(Regular(x / 2), Regular((x + 1) / 2)))
        case Pair(left, right) =>
          split(left).map(Pair(_, right)).orElse(
            split(right).map(Pair(left, _))
          )
        case _ => None
      }

      explode(n, 0).map(_._1).orElse(split(n)) match {
        case Some(reduced) => reduce(reduced)
        case None => n
      }
    }

    reduce(Pair(a, b))
  }

  def addToLeftmost(n: SnailNumber, add: Option[Int]): SnailNumber = (n, add) match {
    case (Regular(x), Some(y)) => Regular(x + y)
    case (Pair(left, right), _) => Pair(addToLeftmost(left, add), right)
    case _ => n
  }

  def addToRightmost(n: SnailNumber, add: Option[Int]): SnailNumber = (n, add) match {
    case (Regular(x), Some(y)) => Regular(x + y)
    case (Pair(left, right), _) => Pair(left, addToRightmost(right, add))
    case _ => n
  }

  def magnitude(n: SnailNumber): Int = n match {
    case Regular(x) => x
    case Pair(left, right) => 3 * magnitude(left) + 2 * magnitude(right)
  }
}

object Solution {
  def main(args: Array[String]): Unit = {
    val numbers = Source.fromFile("input.txt").getLines().map(SnailMath.parse).toList
    val finalSum = numbers.reduce(SnailMath.add)
    println(SnailMath.magnitude(finalSum))
  }
}
