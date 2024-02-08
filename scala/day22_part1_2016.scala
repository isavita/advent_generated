
import scala.io.Source

case class Node(used: Int, avail: Int)

object Main extends App {
  val nodes = readNodes("input.txt")
  val viablePairs = countViablePairs(nodes)
  println(viablePairs)

  def readNodes(filename: String): List[Node] = {
    val source = Source.fromFile(filename)
    val nodeRegex = "node-x\\d+-y\\d+\\s+\\d+T\\s+(\\d+)T\\s+(\\d+)T\\s+\\d+%".r
    val nodes = source.getLines
      .flatMap(line => nodeRegex.findFirstMatchIn(line))
      .map(matches => Node(matches.group(1).toInt, matches.group(2).toInt))
      .toList
    source.close()
    nodes
  }

  def countViablePairs(nodes: List[Node]): Int = {
    nodes.indices.flatMap(i =>
      nodes.indices.collect {
        case j if i != j && nodes(i).used > 0 && nodes(i).used <= nodes(j).avail => 1
      }
    ).sum
  }
}
