
import scala.io.Source

object Solution extends App {
  val filename = "input.txt"

  val (orderingRules, updates) = readInput(filename)

  val sum = updates.map(update => 
    if (isCorrectlyOrdered(update, orderingRules)) update(update.length / 2) else 0
  ).sum

  println(sum)


  def readInput(filename: String): (List[Array[Int]], List[Array[Int]]) = {
    val lines = Source.fromFile(filename).getLines().toList

    val (rulesLines, updatesLines) = lines.span(_ != "")

    val orderingRules = rulesLines.map { line =>
      val parts = line.split("\\|").map(_.trim).map(_.toInt)
      Array(parts(0), parts(1))
    }

    val updates = updatesLines.tail.map { line =>
      line.split(",").map(_.trim).map(_.toInt)
    }

    (orderingRules, updates)
  }

  def isCorrectlyOrdered(update: Array[Int], rules: List[Array[Int]]): Boolean = {
    val position = update.zipWithIndex.toMap

    rules.forall { rule =>
      val x = rule(0)
      val y = rule(1)
      !(position.contains(x) && position.contains(y) && position(x) >= position(y))
    }
  }
}
