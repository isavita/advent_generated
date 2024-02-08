
import scala.io.Source

class Cave {
  var connections: Map[String, Boolean] = Map()

  def connectTo(name: String): Unit = {
    connections += (name -> true)
  }

  def disconnectFrom(name: String): Unit = {
    connections -= name
  }
}

object Solution {
  def main(args: Array[String]): Unit = {
    val caves: collection.mutable.Map[String, Cave] = collection.mutable.Map()
    val source = Source.fromFile("input.txt")

    for (line <- source.getLines) {
      val paths = line.split("-")
      val from = paths(0)
      val to = paths(1)

      if (!caves.contains(from)) {
        caves(from) = new Cave
      }

      if (!caves.contains(to)) {
        caves(to) = new Cave
      }

      caves(from).connectTo(to)
      caves(to).connectTo(from)
    }

    var count = 0

    def dfs(current: String, visited: Map[String, Boolean]): Unit = {
      if (current == "end") {
        count += 1
        return
      }

      for (next <- caves(current).connections.keys) {
        if (visited.contains(next) && next.toLowerCase == next) {
          ()
        } else {
          val visitedCopy = visited + (next -> true)
          dfs(next, visitedCopy)
        }
      }
    }

    dfs("start", Map("start" -> true))
    println(count)
  }
}
