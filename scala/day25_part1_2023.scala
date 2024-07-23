
import scala.io.Source
import scala.collection.mutable

object GraphCut {
  type Vertice = String
  case class Edge(start: Vertice, end: Vertice, weight: Int)

  type Graph = mutable.Map[Vertice, mutable.Set[Edge]]

  def parseInput(input: List[String]): Graph = {
    val weight = 1
    val graph = mutable.Map[Vertice, mutable.Set[Edge]]()

    for (line <- input) {
      val parts = line.split(": ")
      val vertice = parts(0)
      val others = parts(1).split(" ")

      if (!graph.contains(vertice)) graph(vertice) = mutable.Set()
      for (other <- others) {
        if (!graph.contains(other)) graph(other) = mutable.Set()
        graph(vertice) += Edge(vertice, other, weight)
        graph(other) += Edge(other, vertice, weight)
      }
    }
    graph
  }

  def breadthFirstSearch(graph: Graph, start: Vertice, goalFunc: Vertice => Boolean): (Boolean, Map[Vertice, Vertice]) = {
    val frontier = mutable.Queue(start)
    val reached = mutable.Set(start)
    val cameFrom = mutable.Map(start -> start)

    while (frontier.nonEmpty) {
      val current = frontier.dequeue()
      if (goalFunc(current)) return (true, cameFrom.toMap)

      for (next <- graph(current)) {
        if (!reached.contains(next.end)) {
          frontier.enqueue(next.end)
          reached.add(next.end)
          cameFrom(next.end) = current
        }
      }
    }
    (false, cameFrom.toMap)
  }

  def reconstructPath(start: Vertice, end: Vertice, cameFrom: Map[Vertice, Vertice]): List[Vertice] = {
    val path = mutable.ListBuffer[Vertice]()
    var current = end
    while (current != start) {
      path.prepend(current)
      current = cameFrom(current)
    }
    path.prepend(start)
    path.toList
  }

  def copyGraph(graph: Graph): Graph = {
    val newGraph = mutable.Map[Vertice, mutable.Set[Edge]]()
    for ((vertice, edges) <- graph) {
      newGraph(vertice) = edges.clone()
    }
    newGraph
  }

  def solve(input: List[String]): Int = {
    val minCut = 3
    val graph = parseInput(input)

    val source = graph.keys.head
    var separateGraph: Graph = graph.clone()

    for (end <- graph.keys if source != end) {
      val newGraph = copyGraph(graph)
      for (_ <- 0 until minCut) {
        val (_, cameFrom) = breadthFirstSearch(newGraph, source, _ == end)
        val path = reconstructPath(source, end, cameFrom)
        for (j <- path.indices.init) {
          val edge = Edge(path(j), path(j + 1), 1)
          newGraph(path(j)) -= edge
        }
      }

      if (!breadthFirstSearch(newGraph, source, _ == end)._1) {
        separateGraph = newGraph
        return calculateCut(separateGraph, source)
      }
    }
    0
  }

  def calculateCut(graph: Graph, source: Vertice): Int = {
    val (_, cameFrom) = breadthFirstSearch(graph, source, _ => false)
    val length1 = cameFrom.size
    val length2 = graph.size - length1
    length1 * length2
  }

  def readFile(fileName: String): List[String] = {
    Source.fromFile(fileName).getLines().toList
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("input.txt")
    println(solve(input))
  }
}
