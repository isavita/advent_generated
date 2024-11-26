
import scala.io.Source

object DistressSignal {
  // Recursive function to parse a packet string into a nested list structure
  def parsePacket(s: String): Any = {
    def parse(chars: List[Char]): (Any, List[Char]) = {
      chars match {
        case '[' :: rest =>
          var result = List.empty[Any]
          var remaining = rest
          while (remaining.head != ']') {
            val (item, newRemaining) = parse(remaining)
            result = result :+ item
            remaining = newRemaining
            if (remaining.head == ',') remaining = remaining.tail
          }
          (result, remaining.tail)
        case _ =>
          val (numStr, remaining) = chars.span(c => c.isDigit)
          (numStr.mkString.toInt, remaining)
      }
    }
    parse(s.toList)._1
  }

  // Compare two packets recursively
  def comparePackets(left: Any, right: Any): Int = {
    (left, right) match {
      case (l: Int, r: Int) => l.compareTo(r)
      case (l: Int, r: List[_]) => comparePackets(List(l), r)
      case (l: List[_], r: Int) => comparePackets(l, List(r))
      case (l: List[_], r: List[_]) =>
        val comparisons = l.zip(r).map { case (a, b) => comparePackets(a, b) }
        val firstNonZero = comparisons.find(_ != 0)
        firstNonZero.getOrElse(l.length.compareTo(r.length))
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString
    val pairs = input.split("\n\n")
      .map(_.split("\n"))
      .map(pair => (parsePacket(pair(0)), parsePacket(pair(1))))

    val rightOrderIndices = pairs.zipWithIndex
      .filter { case ((left, right), _) => comparePackets(left, right) < 0 }
      .map(_._2 + 1)

    println(rightOrderIndices.sum)
  }
}
