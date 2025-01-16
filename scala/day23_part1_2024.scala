
import scala.io.Source

object Main extends App {
  type Graph = Map[String, Set[String]]

  def addConnection(graph: Graph, from: String, to: String): Graph = {
    graph + (from -> (graph.getOrElse(from, Set.empty) + to))
  }

  def findTripletsWithT(graph: Graph): Int = {
    val computers = graph.keys.toList
    var count = 0
    val seen = collection.mutable.Set[String]()

    for {
      i <- 0 until computers.length
      j <- i + 1 until computers.length
      k <- j + 1 until computers.length
    } {
      val (c1, c2, c3) = (computers(i), computers(j), computers(k))
      if (graph(c1).contains(c2) && graph(c2).contains(c3) && graph(c1).contains(c3)) {
        if (c1.startsWith("t") || c2.startsWith("t") || c3.startsWith("t")) {
          val triplet = List(c1, c2, c3).sorted.mkString(",")
          if (!seen(triplet)) {
            count += 1
            seen += triplet
          }
        }
      }
    }
    count
  }

  val file = Source.fromFile("input.txt")
  val lines = file.getLines().toList
  file.close()

  var graph: Graph = Map.empty

  for (line <- lines) {
    val computers = line.split("-")
    if (computers.length == 2) {
      graph = addConnection(graph, computers(0), computers(1))
      graph = addConnection(graph, computers(1), computers(0))
    }
  }

  val tripletsWithT = findTripletsWithT(graph)
  println(s"Number of triplets containing at least one computer with name starting with 't': $tripletsWithT")
}
