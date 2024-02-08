
import scala.io.Source

object Solution {
  def main(args: Array[String]): Unit = {
    val (template, rules) = readInput("input.txt")
    var pairCounts: Map[String, Long] = Map()
    
    for (i <- 0 until template.length - 1) {
      pairCounts += template.substring(i, i + 2) -> (pairCounts.getOrElse(template.substring(i, i + 2), 0L) + 1)
    }
    
    var step = 0
    while (step < 40) {
      var newPairCounts: Map[String, Long] = Map()
      pairCounts.foreach { case (pair, count) =>
        rules.get(pair) match {
          case Some(insert) => {
            newPairCounts += pair(0).toString + insert -> (newPairCounts.getOrElse(pair(0).toString + insert, 0L) + count)
            newPairCounts += insert + pair(1).toString -> (newPairCounts.getOrElse(insert + pair(1).toString, 0L) + count)
          }
          case None => newPairCounts += pair -> (newPairCounts.getOrElse(pair, 0L) + count)
        }
      }
      pairCounts = newPairCounts
      step += 1
    }
    
    var elementCounts: Map[Char, Long] = Map()
    pairCounts.foreach { case (pair, count) =>
      elementCounts += pair(0) -> (elementCounts.getOrElse(pair(0), 0L) + count)
    }
    elementCounts += template(template.length - 1) -> (elementCounts.getOrElse(template(template.length - 1), 0L) + 1)
    
    var maxCount = 0L
    var minCount = Long.MaxValue
    elementCounts.values.foreach { count =>
      maxCount = Math.max(maxCount, count)
      minCount = Math.min(minCount, count)
    }
    
    println(maxCount - minCount)
  }
  
  def readInput(filename: String): (String, Map[String, String]) = {
    val lines = Source.fromFile(filename).getLines().toList
    val template = lines.head
    val rules = lines.tail.filter(_.nonEmpty).map { line =>
      val parts = line.split(" -> ")
      parts(0) -> parts(1)
    }.toMap
    (template, rules)
  }
}
