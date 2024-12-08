
import scala.util.matching.Regex

object Day3 extends App {

  def solvePart1(input: String): Long = {
    val mulRegex = """mul\((-?\d+),(-?\d+)\)""".r
    mulRegex.findAllIn(input).matchData.map { m =>
      m.group(1).toLong * m.group(2).toLong
    }.sum
  }

  def solvePart2(input: String): Long = {
    var enabled = true
    var sum = 0L
    val mulRegex = """mul\((-?\d+),(-?\d+)\)""".r
    val doRegex = """do\(\)""".r
    val dontRegex = """don't\(\)""".r

    val instructions = (mulRegex.findAllIn(input).matchData ++ doRegex.findAllIn(input).matchData ++ dontRegex.findAllIn(input).matchData).toSeq.sortBy(_.start)

    for (m <- instructions) {
      m.matched match {
        case s if mulRegex.pattern.matcher(s).matches =>
          if (enabled) {
            val parts = s.substring(4, s.length - 1).split(",")
            sum += parts(0).toLong * parts(1).toLong
          }
        case "do()" => enabled = true
        case "don't()" => enabled = false
        case _ => // Ignore other matches
      }
    }
    sum
  }


  val input = scala.io.Source.fromFile("input.txt").mkString

  println(s"Part 1: ${solvePart1(input)}")
  println(s"Part 2: ${solvePart2(input)}")
}
