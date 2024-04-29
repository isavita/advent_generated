import scala.io.Source
import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString.trim
    println(someAssemblyRequired(input))
  }

  def someAssemblyRequired(input: String): Int = {
    val wireToRule = input.split("\n").map { inst =>
      val parts = inst.split(" -> ")
      parts(1) -> parts(0)
    }.to(mutable.Map)

    val aSignal = memoDFS(wireToRule, "a", mutable.Map.empty[String, Int])
    wireToRule.update("b", aSignal.toString)
    memoDFS(wireToRule, "a", mutable.Map.empty)
  }

  def memoDFS(graph: mutable.Map[String, String], entry: String, memo: mutable.Map[String, Int]): Int = {
    if (memo.contains(entry)) {
      memo(entry)
    } else {
      val result = entry match {
        case num if num forall (_.isDigit) => num.toInt
        case _ =>
          val sourceRule = graph(entry)
          val parts = sourceRule.split(" ")
          val result = parts.length match {
            case 1 => memoDFS(graph, parts(0), memo)
            case 2 if parts(0) == "NOT" => ~memoDFS(graph, parts(1), memo)
            case 3 if parts(1) == "AND" => memoDFS(graph, parts(0), memo) & memoDFS(graph, parts(2), memo)
            case 3 if parts(1) == "OR" => memoDFS(graph, parts(0), memo) | memoDFS(graph, parts(2), memo)
            case 3 if parts(1) == "LSHIFT" => memoDFS(graph, parts(0), memo) << memoDFS(graph, parts(2), memo)
            case 3 if parts(1) == "RSHIFT" => memoDFS(graph, parts(0), memo) >> memoDFS(graph, parts(2), memo)
            case _ => throw new Exception("Invalid operation")
          }
          memo.update(entry, result)
          result
      }
      result
    }
  }
}