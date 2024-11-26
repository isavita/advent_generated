
import scala.io.Source

object Day19 {
  case class Part(x: Int, m: Int, a: Int, s: Int) {
    def rating: Int = x + m + a + s
  }

  case class Rule(category: Option[Char], comparison: Option[Char], value: Option[Int], destination: String)

  def parseWorkflows(lines: List[String]): Map[String, List[Rule]] = {
    lines.map { line =>
      val name = line.takeWhile(_ != '{')
      val rules = line.drop(name.length + 1).dropRight(1).split(',').map { ruleStr =>
        if (ruleStr.contains(':')) {
          val Array(condition, dest) = ruleStr.split(':')
          val category = condition.head
          val comparison = condition(1)
          val value = condition.drop(2).toInt
          Rule(Some(category), Some(comparison), Some(value), dest)
        } else {
          Rule(None, None, None, ruleStr)
        }
      }.toList
      name -> rules
    }.toMap
  }

  def processPart(part: Part, workflows: Map[String, List[Rule]]): Boolean = {
    def processWorkflow(name: String): Boolean = {
      if (name == "A") return true
      if (name == "R") return false

      val workflow = workflows(name)
      val nextWorkflow = workflow.find { rule =>
        rule.category.forall { cat =>
          val value = cat match {
            case 'x' => part.x
            case 'm' => part.m
            case 'a' => part.a
            case 's' => part.s
          }
          rule.comparison.forall {
            case '>' => value > rule.value.get
            case '<' => value < rule.value.get
          }
        }
      }.get

      processWorkflow(nextWorkflow.destination)
    }

    processWorkflow("in")
  }

  def countAcceptedCombinations(workflows: Map[String, List[Rule]]): Long = {
    def explore(workflow: String, ranges: Map[Char, (Int, Int)]): Long = {
      if (workflow == "A") return ranges.values.map { case (min, max) => max - min + 1L }.product
      if (workflow == "R") return 0L

      var total = 0L
      var currentRanges = ranges

      workflows(workflow).foreach { rule =>
        rule.category match {
          case Some(cat) =>
            val (min, max) = currentRanges(cat)
            val (trueRange, falseRange) = rule.comparison match {
              case Some('>') =>
                ((rule.value.get + 1, max), (min, rule.value.get))
              case Some('<') =>
                ((min, rule.value.get - 1), (rule.value.get, max))
            }

            val trueRanges = currentRanges + (cat -> trueRange)
            total += explore(rule.destination, trueRanges)

            currentRanges = currentRanges + (cat -> falseRange)
          case None =>
            total += explore(rule.destination, currentRanges)
        }
      }

      total
    }

    explore("in", Map('x' -> (1, 4000), 'm' -> (1, 4000), 'a' -> (1, 4000), 's' -> (1, 4000)))
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    val (workflowLines, partLines) = input.span(_.nonEmpty)

    val workflows = parseWorkflows(workflowLines)
    val parts = partLines.drop(1).map { line =>
      val values = line.drop(1).dropRight(1).split(',').map(_.split('=')(1).toInt)
      Part(values(0), values(1), values(2), values(3))
    }

    // Part 1
    val acceptedParts = parts.filter(processPart(_, workflows))
    println(s"Part 1: ${acceptedParts.map(_.rating).sum}")

    // Part 2
    println(s"Part 2: ${countAcceptedCombinations(workflows)}")
  }
}
