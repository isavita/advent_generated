
import scala.io.Source
import scala.collection.mutable

object MaximalClique {
  val graph: mutable.Map[String, mutable.Set[String]] = mutable.Map.empty
  var bestClique: List[String] = List.empty

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toList
    val nodesSet: mutable.Set[String] = mutable.Set.empty

    for (line <- lines) {
      val parts = line.split("-")
      if (parts.length == 2) {
        val a = parts(0)
        val b = parts(1)
        graph.getOrElseUpdate(a, mutable.Set.empty) += b
        graph.getOrElseUpdate(b, mutable.Set.empty) += a
        nodesSet += a
        nodesSet += b
      }
    }

    val allNodes = nodesSet.toList
    bronKerbosch(List.empty, allNodes, List.empty)
    println(bestClique.sorted.mkString(","))
  }

  def bronKerbosch(r: List[String], p: List[String], x: List[String]): Unit = {
    if (p.isEmpty && x.isEmpty) {
      if (r.length > bestClique.length) {
        bestClique = r
      }
      return
    }

    var currentP = p
    var currentX = x
    for (v <- p) {
      val neighbors = graph.getOrElse(v, mutable.Set.empty)
      bronKerbosch(
        r :+ v,
        currentP.filter(neighbors.contains),
        currentX.filter(neighbors.contains)
      )
      currentP = currentP.filter(_ != v)
      currentX = currentX :+ v
    }
  }
}
