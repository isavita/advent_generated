
import scala.io.Source
import scala.util.Using
import scala.collection.mutable

object Solution {
  def main(args: Array[String]): Unit = {
    val sum = Using(Source.fromFile("input.txt")) { source =>
      source.getLines().map(evaluate).sum
    }.getOrElse(0L)
    println(sum)
  }

  def evaluate(expression: String): Long = {
    val tokens = expression.replace("(", "( ").replace(")", " )").split("\\s+").toSeq
    evaluateTokens(tokens)
  }

  def evaluateTokens(tokens: Seq[String]): Long = {
    val ops = mutable.Stack[String]()
    val vals = mutable.Stack[Long]()

    for (token <- tokens) {
      token match {
        case "(" => ops.push(token)
        case "+" | "*" =>
          while (ops.nonEmpty && ops.head != "(") {
            val b = vals.pop()
            val a = vals.pop()
            val op = ops.pop()
            vals.push(applyOp(op, a, b))
          }
          ops.push(token)
        case ")" =>
          while (ops.head != "(") {
            val b = vals.pop()
            val a = vals.pop()
            val op = ops.pop()
            vals.push(applyOp(op, a, b))
          }
          ops.pop()
        case _ => vals.push(token.toLong)
      }
    }
    while (ops.nonEmpty) {
      val b = vals.pop()
      val a = vals.pop()
      val op = ops.pop()
      vals.push(applyOp(op, a, b))
    }
    vals.pop()
  }

  def applyOp(op: String, a: Long, b: Long): Long = {
    op match {
      case "+" => a + b
      case "*" => a * b
      case _ => throw new IllegalArgumentException(s"Unknown operator: $op")
    }
  }
}
