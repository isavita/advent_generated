import scala.io.Source
import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = {
    val adj = mutable.Map[String, List[String]]()
    for (line <- Source.fromFile("input.txt").getLines()) {
      val l = line.trim
      if (l.nonEmpty) {
        val parts = l.split(":")
        if (parts.length == 2) {
          val src = parts(0).trim
          val dests = parts(1).trim.split("\\s+").filter(_.nonEmpty).toList
          adj(src) = dests
        }
      }
    }

    def countPaths(start: String, end: String): Long = {
      val memo = mutable.Map[String, Long]()
      def dfs(cur: String): Long = {
        if (cur == end) 1L
        else memo.getOrElseUpdate(cur, adj.getOrElse(cur, Nil).map(dfs).sum)
      }
      dfs(start)
    }

    val s1 = countPaths("svr", "dac") * countPaths("dac", "fft") * countPaths("fft", "out")
    val s2 = countPaths("svr", "fft") * countPaths("fft", "dac") * countPaths("dac", "out")
    println(s"Paths (svr->dac->fft->out): $s1")
    println(s"Paths (svr->fft->dac->out): $s2")
    println(s"Total paths visiting both: ${s1 + s2}")
  }
}