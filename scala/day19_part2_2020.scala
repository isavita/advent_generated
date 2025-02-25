
import scala.io.Source
import scala.util.matching.Regex

object Solution {

  case class Rule(var resolved: List[String] = Nil, options: List[List[Int]] = Nil)

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString.trim
    val result = solve(input)
    println(result)
  }

  def solve(inputData: String): Int = {
    val (graph, messages) = parseInput(inputData)

    fillInGraph(graph, 42)
    fillInGraph(graph, 31)

    val part42 = s"(${graph(42).resolved.mkString("|")})"
    val part31 = s"(${graph(31).resolved.mkString("|")})"

    val rule8String = s"($part42)+"

    def makeRegexp(num: Int): Regex = {
      s"^$rule8String$part42{$num}$part31{$num}$$".r
    }

    (for {
      m <- messages
      i <- 1 until 10
      pattern = makeRegexp(i)
      if pattern.matches(m)
    } yield 1).sum
  }

  def fillInGraph(graph: Map[Int, Rule], entry: Int): List[String] = {
    if (graph(entry).resolved.nonEmpty) {
      return graph(entry).resolved
    }

    for (option <- graph(entry).options) {
      var resolved = List("")
      for (entryPoint <- option) {
        val nestedResolveVals = fillInGraph(graph, entryPoint)
        val newResolved = for {
          nextPiece <- nestedResolveVals
          prev      <- resolved
        } yield prev + nextPiece
        resolved = newResolved
      }
      graph(entry).resolved = graph(entry).resolved ++ resolved
    }
    graph(entry).resolved
  }

  def parseInput(inputData: String): (Map[Int, Rule], List[String]) = {
    val parts = inputData.split("\n\n")

    val rules = parts(0).split("\n").map { r =>
      if (r.contains("\"")) {
        val parts_ = r.split(": ")
        val num = parts_(0).toInt
        val char = parts_(1).drop(1).dropRight(1)

        num -> Rule(resolved = List(char))
      } else {
        val Array(keyStr, ruleNumsStr) = r.split(": ")
        val key = keyStr.toInt
        val options = ruleNumsStr.split(" \\| ").map(_.split(" ").map(_.toInt).toList).toList
        key -> Rule(options = options)
      }
    }.toMap

    val messages = parts(1).split("\n").toList

    (rules, messages)
  }
}
