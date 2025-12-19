import scala.io.Source
import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines()
    val adj = mutable.Map.empty[String, Array[String]]
    for (line <- lines.map(_.trim).filter(_.nonEmpty)) {
      val parts = line.split(":")
      if (parts.length == 2) {
        val src = parts(0).trim
        val dests = parts(1).trim.split("\\s+")
        adj(src) = dests
      }
    }
    val memo = mutable.Map.empty[String, Long]
    val count = countPaths("you", "out", adj, memo)
    println(s"Number of paths from 'you' to 'out': $count")
  }

  def countPaths(cur: String, target: String, adj: mutable.Map[String, Array[String]], memo: mutable.Map[String, Long]): Long = {
    if (cur == target) return 1L
    memo.getOrElseUpdate(cur, adj.get(cur) match {
      case Some(nei) => nei.map(n => countPaths(n, target, adj, memo)).sum
      case None => 0L
    })
  }
}