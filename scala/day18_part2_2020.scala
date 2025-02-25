
object MathSolver {

  def evaluateSimple(expression: String): Long = {
    val parts = expression.split("(?<=[+*])|(?=[+*])").filter(_.nonEmpty)
    var total = parts(0).toLong
    for (i <- 1 until parts.length by 2) {
      parts(i) match {
        case "+" => total += parts(i + 1).toLong
        case "*" => total *= parts(i + 1).toLong
      }
    }
    total
  }

  def evaluateAdvanced(expression: String): Long = {
    var parts = expression.split("(?<=[+*])|(?=[+*])").filter(_.nonEmpty).toList
    while (parts.contains("+")) {
      val i = parts.indexOf("+")
      val subResult = parts(i - 1).toLong + parts(i + 1).toLong
      parts = parts.take(i - 1) ::: (subResult.toString :: parts.drop(i + 2))
    }
    var total = parts.head.toLong
    for (i <- 1 until parts.length by 2) {
        total *= parts(i+1).toLong
    }
    total
  }

  def evaluateExpression(expression: String, evaluateFn: String => Long): Long = {
    var expr = expression
    while (expr.contains('(')) {
      val start = expr.lastIndexOf('(')
      val end = start + expr.substring(start).indexOf(')')
      expr = expr.substring(0, start) + evaluateFn(expr.substring(start + 1, end)).toString + expr.substring(end + 1)
    }
    evaluateFn(expr)
  }

  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.fromFile("input.txt").getLines().toList
    val resultPart1 = lines.map(line => evaluateExpression(line.replace(" ", ""), evaluateSimple)).sum
    val resultPart2 = lines.map(line => evaluateExpression(line.replace(" ", ""), evaluateAdvanced)).sum

    println(resultPart1)
    println(resultPart2)
  }
}
