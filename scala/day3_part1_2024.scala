
import scala.util.matching.Regex

object Day3 extends App {

  val input = scala.io.Source.fromFile("input.txt").mkString

  val mulRegex: Regex = """mul\s*\(\s*(\d+)\s*,\s*(\d+)\s*\)""".r

  val results = mulRegex.findAllIn(input).matchData.map { m =>
    val num1 = m.group(1).toInt
    val num2 = m.group(2).toInt
    num1 * num2
  }.toList

  val sumOfResults = results.sum

  println(s"Sum of multiplication results: $sumOfResults")
}
