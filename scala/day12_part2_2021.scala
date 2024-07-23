
import scala.io.Source

object Main extends App {
  val input = Source.fromFile("input.txt").getLines().mkString("\n").trim
  println(solve(input))

  def solve(input: String): Int = {
    val parsed = parseInput(input)
    val graph = parsed.foldLeft(Map.empty[String, Set[String]]) { (acc, pair) =>
      acc + (pair(0) -> (acc.getOrElse(pair(0), Set.empty) + pair(1))) +
        (pair(1) -> (acc.getOrElse(pair(1), Set.empty) + pair(0)))
    }
    walk(graph, "start", Map("start" -> 5), List("start"), false)
  }

  def walk(graph: Map[String, Set[String]], current: String, visited: Map[String, Int], path: List[String], doubleUsed: Boolean): Int = {
    if (current == "end") return 1

    val updatedVisited = visited.updated(current, visited.getOrElse(current, 0) + 1)
    graph(current).foldLeft(0) { (acc, visitable) =>
      if (visitable == "start") acc
      else {
        val isUpper = visitable.toUpperCase == visitable
        if (!isUpper && updatedVisited.getOrElse(visitable, 0) > 0) {
          if (doubleUsed) acc
          else walk(graph, visitable, updatedVisited, visitable :: path, true) + acc
        } else {
          walk(graph, visitable, updatedVisited, visitable :: path, doubleUsed) + acc
        }
      }
    }
  }

  def parseInput(input: String): Array[Array[String]] = {
    input.split("\n").map(_.split("-"))
  }
}
