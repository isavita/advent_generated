object Solution extends App {
  import scala.io.Source
  import scala.util.matching.Regex

  case class Rule(name: String, ranges: List[(Int, Int)]) {
    def isValid(value: Int): Boolean = ranges.exists { case (min, max) => value >= min && value <= max }
  }

  val source = Source.fromFile("input.txt")
  val lines = source.getLines.toList
  source.close()

  val reRule: Regex = raw"([^:]+): (\d+)-(\d+) or (\d+)-(\d+)".r
  var rules: List[Rule] = List()
  var scanningRules = true
  var errorRate = 0

  lines.foreach { line =>
    if (line.nonEmpty) {
      if (line.startsWith("your ticket:") || line.startsWith("nearby tickets:")) {
        scanningRules = false
      } else if (scanningRules) {
        reRule.findFirstMatchIn(line).foreach { m =>
          val name = m.group(1)
          val range1 = (m.group(2).toInt, m.group(3).toInt)
          val range2 = (m.group(4).toInt, m.group(5).toInt)
          rules = rules :+ Rule(name, List(range1, range2))
        }
      } else {
        line.split(",").foreach { value =>
          val intValue = value.toInt
          if (!isValidForAnyRule(intValue, rules)) {
            errorRate += intValue
          }
        }
      }
    }
  }

  println(errorRate)

  def isValidForAnyRule(value: Int, rules: List[Rule]): Boolean = rules.exists(_.isValid(value))
}