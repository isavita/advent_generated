
import scala.io.Source
import scala.util.matching.Regex

object Assembly {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().mkString("\n").trim
    println(someAssemblyRequired(input))
  }

  def someAssemblyRequired(input: String): Int = {
    val wireToRule = input.linesIterator.map { inst =>
      val parts = inst.split(" -> ")
      parts(1) -> parts(0)
    }.toMap

    memoDFS(wireToRule, "a", collection.mutable.Map.empty)
  }

  def memoDFS(graph: Map[String, String], entry: String, memo: collection.mutable.Map[String, Int]): Int = {
    memo.get(entry).getOrElse {
      val result = if (entry.forall(_.isDigit)) {
        entry.toInt
      } else {
        val sourceRule = graph(entry)
        val parts = sourceRule.split(" ")

        parts.length match {
          case 1 => memoDFS(graph, parts(0), memo)
          case _ => parts(0) match {
            case "NOT" => (~memoDFS(graph, parts(1), memo)) & 0xFFFF
            case _ =>
              val left = memoDFS(graph, parts(0), memo)
              val right = memoDFS(graph, parts(2), memo)
              parts(1) match {
                case "AND" => left & right
                case "OR"  => left | right
                case "LSHIFT" => left << right
                case "RSHIFT" => left >> right
              }
          }
        }
      }
      memo(entry) = result
      result
    }
  }
}
