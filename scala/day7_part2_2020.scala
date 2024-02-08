
import scala.io.Source

case class BagRule(color: String, count: Int)

object Main extends App {
  val rules = Source.fromFile("input.txt").getLines()
    .map(line => {
      val parts = line.split(" bags contain ")
      val container = parts(0)
      val contents = parts(1)

      if (contents == "no other bags.") {
        (container, List[BagRule]())
      } else {
        val ruleRegex = raw"(\d+) (\w+ \w+) bags?[,.]".r
        val bagRules = ruleRegex.findAllIn(contents).matchData.map(m => BagRule(m.group(2), m.group(1).toInt)).toList
        (container, bagRules)
      }
    })
    .toMap

  val totalBags = countBags("shiny gold", rules) - 1
  println(totalBags)

  def countBags(color: String, rules: Map[String, List[BagRule]]): Int = {
    1 + rules(color).map(rule => rule.count * countBags(rule.color, rules)).sum
  }
}
