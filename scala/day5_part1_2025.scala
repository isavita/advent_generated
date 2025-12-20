import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val it = Source.fromFile("input.txt").getLines()
    var parsing = true
    val ranges = scala.collection.mutable.ArrayBuffer[(Long, Long)]()
    val ids = scala.collection.mutable.ArrayBuffer[Long]()
    for (line <- it) {
      val s = line.trim
      if (s.isEmpty) {
        if (parsing) parsing = false
      } else if (parsing) {
        val parts = s.split("-")
        ranges += ((parts(0).toLong, parts(1).toLong))
      } else {
        ids += s.toLong
      }
    }
    val merged = {
      val sorted = ranges.sortBy(_._1)
      val buf = scala.collection.mutable.ArrayBuffer[(Long, Long)]()
      for ((mn, mx) <- sorted) {
        if (buf.isEmpty || mn > buf.last._2) buf += ((mn, mx))
        else if (mx > buf.last._2) buf(buf.size - 1) = (buf.last._1, mx)
      }
      buf.toArray
    }
    def contains(x: Long): Boolean = {
      var l = 0
      var r = merged.length
      while (l < r) {
        val m = (l + r) >>> 1
        if (x < merged(m)._1) r = m
        else if (x > merged(m)._2) l = m + 1
        else return true
      }
      false
    }
    val fresh = ids.count(contains)
    println(s"Number of fresh ingredients: $fresh")
  }
}