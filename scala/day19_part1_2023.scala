
import scala.io.Source

object Aplenty {
  case class Part(x: Int, m: Int, a: Int, s: Int) {
    def rating: Int = x + m + a + s
  }

  case class Rule(category: Option[Char], comparison: Option[Char], value: Option[Int], destination: String) {
    def matches(part: Part): Boolean = {
      category match {
        case Some('x') => comparison match {
          case Some('<') => part.x < value.get
          case Some('>') => part.x > value.get
          case _ => true
        }
        case Some('m') => comparison match {
          case Some('<') => part.m < value.get
          case Some('>') => part.m > value.get
          case _ => true
        }
        case Some('a') => comparison match {
          case Some('<') => part.a < value.get
          case Some('>') => part.a > value.get
          case _ => true
        }
        case Some('s') => comparison match {
          case Some('<') => part.s < value.get
          case Some('>') => part.s > value.get
          case _ => true
        }
        case _ => true
      }
    }
  }

  def parseWorkflow(line: String): (String, List[Rule]) = {
    val parts = line.split('{')
    val name = parts(0)
    val rules = parts(1).dropRight(1).split(',').map { ruleStr =>
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
    (name, rules)
  }

  def parsePart(line: String): Part = {
    val ratings = line.drop(1).dropRight(1).split(',').map { rating =>
      val Array(_, value) = rating.split('=')
      value.toInt
    }
    Part(ratings(0), ratings(1), ratings(2), ratings(3))
  }

  def processWorkflows(part: Part, workflows: Map[String, List[Rule]]): Boolean = {
    def process(currentWorkflow: String): Boolean = {
      if (currentWorkflow == "A") true
      else if (currentWorkflow == "R") false
      else {
        val rules = workflows(currentWorkflow)
        val matchedRule = rules.find(_.matches(part))
        process(matchedRule.get.destination)
      }
    }
    process("in")
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toList
    val (workflowLines, partLines) = lines.span(_.nonEmpty)

    val workflows = workflowLines.map(parseWorkflow).toMap
    val parts = partLines.drop(1).map(parsePart)

    val acceptedParts = parts.filter(processWorkflows(_, workflows))
    val totalRating = acceptedParts.map(_.rating).sum

    println(s"Total rating of accepted parts: $totalRating")
  }
}
