
import scala.io.Source
import scala.collection.mutable.{Map, Set}

object Solution extends App {
  val filename = "input.txt"
  val (orderingRules, updates) = readInput(filename)

  val sum = updates.foldLeft(0) { (acc, update) =>
    if (!isCorrectlyOrdered(update, orderingRules)) {
      val sortedUpdate = sortUpdate(update, orderingRules)
      acc + sortedUpdate(sortedUpdate.length / 2)
    } else acc
  }

  println(sum)


  def readInput(filename: String): (List[Array[Int]], List[Array[Int]]) = {
    val lines = Source.fromFile(filename).getLines().toList
    val (rulesLines, updatesLines) = lines.span(_ != "")
    val orderingRules = rulesLines.map(_.split("\\|").map(_.trim.toInt).toArray).filter(_.length == 2)
    val updates = updatesLines.tail.map(_.split(",").map(_.trim.toInt).toArray).filter(_.nonEmpty)
    (orderingRules, updates)
  }

  def isCorrectlyOrdered(update: Array[Int], rules: List[Array[Int]]): Boolean = {
    val position = update.zipWithIndex.toMap
    rules.forall { case Array(x, y) =>
      !position.contains(x) || !position.contains(y) || position(x) < position(y)
    }
  }

  def sortUpdate(update: Array[Int], rules: List[Array[Int]]): Array[Int] = {
    val adjacency = Map.from(update.map(x => (x, List.empty[Int])) )
    val pagesInUpdate = Set(update:_*)

    rules.foreach { case Array(x, y) =>
      if (pagesInUpdate.contains(x) && pagesInUpdate.contains(y)) {
        adjacency(x) = adjacency(x) :+ y
      }
    }

    var visited = Set.empty[Int]
    var result = List.empty[Int]
    def visit(n: Int): Unit = {
      if (!visited.contains(n)) {
        visited += n
        adjacency(n).foreach(visit)
        result ::= n
      }
    }
    pagesInUpdate.foreach(visit)
    result.toArray
  }
}
