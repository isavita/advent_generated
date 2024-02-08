
import scala.io.Source

object Day19 extends App {
  val input = Source.fromFile("input.txt").getLines().toList

  val (rules, messages) = input.span(_.nonEmpty)

  val ruleMap = rules.map { rule =>
    val Array(ruleNum, ruleDef) = rule.split(": ")
    ruleNum.toInt -> ruleDef
  }.toMap

  def parseRule(ruleNum: Int): String = {
    val ruleDef = ruleMap(ruleNum)
    if (ruleDef.startsWith("\"")) ruleDef(1).toString
    else ruleDef.split(" \\| ").map { subRule =>
      subRule.split(" ").map(_.toInt).map(parseRule).mkString
    }.mkString("(", "|", ")")
  }

  val regex = ("^" + parseRule(0) + "$").r

  val result = messages.count { message =>
    regex.matches(message)
  }

  println(result)
}
