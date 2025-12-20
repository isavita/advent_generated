
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import java.util.Arrays

object Main {
  def bits(l: Long): Int = java.lang.Long.bitCount(l).toInt

  def minWeight(mat: Array[Array[Int]], R: Int, C: Int): Int = {
    val m = mat.map(_.clone())
    val colPivot = new Array[Boolean](C)
    var piv = 0
    for (c <- 0 until C if piv < R) {
      var sel = -1
      var r = piv
      while (r < R && sel == -1) {
        if (m(r)(c) == 1) sel = r
        r += 1
      }
      if (sel != -1) {
        val tmp = m(piv); m(piv) = m(sel); m(sel) = tmp
        for (r2 <- 0 until R if r2 != piv && m(r2)(c) == 1) {
          var k = c
          while (k <= C) { m(r2)(k) ^= m(piv)(k); k += 1 }
        }
        colPivot(c) = true
        piv += 1
      }
    }
    for (r <- piv until R) if (m(r)(C) == 1) return -1
    val free = (0 until C).filterNot(colPivot).toArray
    val nFree = free.length
    var best = Int.MaxValue
    val limit = 1L << nFree
    val x = new Array[Int](C)
    var mask = 0L
    while (mask < limit) {
      Arrays.fill(x, 0)
      var w = bits(mask)
      var j = 0
      while (j < nFree) {
        if ((mask >> j & 1L) == 1L) x(free(j)) = 1
        j += 1
      }
      var prow = 0
      for (c <- 0 until C if colPivot(c)) {
        var v = m(prow)(C)
        var k = c + 1
        while (k < C) {
          if (m(prow)(k) == 1) v ^= x(k)
          k += 1
        }
        x(c) = v
        if (v == 1) w += 1
        prow += 1
      }
      if (w < best) best = w
      mask += 1
    }
    best
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines()
    var total = 0
    val btnPat = """\(([^)]*)\)""".r
    for (raw <- lines) {
      val line = raw.trim
      val lb = line.indexOf('[')
      val rb = line.indexOf(']', lb)
      if (lb == -1 || rb == -1) { /* ignore */ }
      else {
        val targetStr = line.substring(lb + 1, rb)
        val R = targetStr.length
        val target = Array.tabulate(R)(i => if (targetStr(i) == '#') 1 else 0)
        val btnMatches = btnPat.findAllMatchIn(line.substring(rb + 1)).toSeq
        val buttons = btnMatches.map { m =>
          val s = m.group(1).trim
          if (s.isEmpty) Array.emptyIntArray
          else s.split(",").map(_.trim.toInt)
        }
        val C = buttons.length
        val matrix = Array.ofDim[Int](R, C + 1)
        for (r <- 0 until R) {
          for (c <- 0 until C) {
            matrix(r)(c) = if (buttons(c).contains(r)) 1 else 0
          }
          matrix(r)(C) = target(r)
        }
        val mw = minWeight(matrix, R, C)
        if (mw != -1) total += mw
      }
    }
    println(total)
  }
}
