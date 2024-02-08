
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList

  val rules = input.map(parseRule)
  val graph = rules.map(rule => (rule._1, rule._2)).toMap

  def parseRule(rule: String): (String, List[(Int, String)]) = {
    val parts = rule.split(" bags contain ")
    val color = parts(0)
    val contents = parts(1).stripSuffix(".").split(", ").toList
    if (contents.head == "no other bags") {
      (color, List())
    } else {
      val parsedContents = contents.map(content => {
        val num = content.head.toString.toInt
        val bagColor = content.substring(2, content.indexOf(" bag"))
        (num, bagColor)
      })
      (color, parsedContents)
    }
  }

  def containsShinyGold(color: String): Boolean = {
    if (graph(color).isEmpty) {
      false
    } else if (graph(color).exists(_._2 == "shiny gold")) {
      true
    } else {
      graph(color).exists(x => containsShinyGold(x._2))
    }
  }

  val result = graph.keys.count(containsShinyGold)
  println(result)
}
