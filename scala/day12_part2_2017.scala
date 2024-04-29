import scala.io.Source
import scala.collection.mutable

object Main {
  def dfs(node: Int, adj: mutable.Map[Int, mutable.Buffer[Int]], visited: mutable.Set[Int]): Unit = {
    visited += node
    for (neighbor <- adj(node)) {
      if (!visited.contains(neighbor)) {
        dfs(neighbor, adj, visited)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val adj = mutable.Map[Int, mutable.Buffer[Int]]()
    val visited = mutable.Set[Int]()
    var groups = 0

    for (line <- Source.fromFile("input.txt").getLines) {
      val parts = line.split(" <-> ")
      val from = parts(0).toInt
      val toNodes = parts(1).split(", ")

      adj.getOrElseUpdate(from, mutable.Buffer[Int]())
      for (toNode <- toNodes) {
        val to = toNode.toInt
        adj(from) += to
        adj.getOrElseUpdate(to, mutable.Buffer[Int]()) += from
      }
    }

    for (node <- adj.keys) {
      if (!visited.contains(node)) {
        dfs(node, adj, visited)
        groups += 1
      }
    }

    println(groups)
  }
}