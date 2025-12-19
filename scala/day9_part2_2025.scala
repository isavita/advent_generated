
import java.io.{File, BufferedReader, FileReader}
import scala.collection.mutable.{ArrayBuffer, Queue, Map, Set}
import scala.math.abs

object Main {
  case class Pt(x: Int, y: Int)

  def main(args: Array[String]): Unit = {
    val br = new BufferedReader(new FileReader(new File("input.txt")))
    val points = ArrayBuffer.empty[Pt]
    val uniqX = Set.empty[Int]
    val uniqY = Set.empty[Int]
    var line = br.readLine()
    while (line != null) {
      val s = line.trim
      if (s.nonEmpty) {
        val parts = s.split(",")
        if (parts.length == 2) {
          val x = parts(0).toInt
          val y = parts(1).toInt
          points += Pt(x, y)
          uniqX += x
          uniqY += y
        }
      }
      line = br.readLine()
    }
    br.close()
    if (points.isEmpty) {
      println("No points found.")
      return
    }

    val xs = uniqX.toArray.sorted
    val ys = uniqY.toArray.sorted
    val xIdx = Map.empty[Int, Int]
    val yIdx = Map.empty[Int, Int]
    xs.indices.foreach(i => xIdx(xs(i)) = i)
    ys.indices.foreach(i => yIdx(ys(i)) = i)

    val w = 2 * xs.length + 1
    val h = 2 * ys.length + 1
    val colW = Array.ofDim[Long](w)
    val rowH = Array.ofDim[Long](h)

    colW(0) = 1
    for (i <- xs.indices) {
      colW(2 * i + 1) = 1
      if (i < xs.length - 1) {
        val gap = xs(i + 1) - xs(i) - 1
        colW(2 * i + 2) = if (gap > 0) gap else 0
      } else colW(2 * i + 2) = 1
    }

    rowH(0) = 1
    for (i <- ys.indices) {
      rowH(2 * i + 1) = 1
      if (i < ys.length - 1) {
        val gap = ys(i + 1) - ys(i) - 1
        rowH(2 * i + 2) = if (gap > 0) gap else 0
      } else rowH(2 * i + 2) = 1
    }

    val grid = Array.ofDim[Byte](h, w)

    def toGrid(p: Pt): (Int, Int) = {
      val gx = 2 * xIdx(p.x) + 1
      val gy = 2 * yIdx(p.y) + 1
      (gx, gy)
    }

    for (i <- points.indices) {
      val p1 = points(i)
      val p2 = points((i + 1) % points.length)
      val (gx1, gy1) = toGrid(p1)
      val (gx2, gy2) = toGrid(p2)
      if (gx1 == gx2) {
        val (s, e) = if (gy1 <= gy2) (gy1, gy2) else (gy2, gy1)
        for (y <- s to e) if (rowH(y) > 0) grid(y)(gx1) = 1
      } else {
        val (s, e) = if (gx1 <= gx2) (gx1, gx2) else (gx2, gx1)
        for (x <- s to e) if (colW(x) > 0) grid(gy1)(x) = 1
      }
    }

    val q = Queue.empty[Pt]
    q.enqueue(Pt(0, 0))
    grid(0)(0) = 2
    val dirs = Array((0, 1), (0, -1), (1, 0), (-1, 0))
    while (q.nonEmpty) {
      val cur = q.dequeue()
      for ((dx, dy) <- dirs) {
        val nx = cur.x + dx
        val ny = cur.y + dy
        if (nx >= 0 && nx < w && ny >= 0 && ny < h && grid(ny)(nx) == 0) {
          grid(ny)(nx) = 2
          q.enqueue(Pt(nx, ny))
        }
      }
    }

    val pref = Array.ofDim[Long](h, w)
    for (y <- 0 until h) {
      var rowSum = 0L
      for (x <- 0 until w) {
        val valArea = if (grid(y)(x) != 2) colW(x) * rowH(y) else 0L
        rowSum += valArea
        val above = if (y > 0) pref(y - 1)(x) else 0L
        pref(y)(x) = rowSum + above
      }
    }

    def getSum(x1: Int, y1: Int, x2: Int, y2: Int): Long = {
      val (lx, rx) = if (x1 <= x2) (x1, x2) else (x2, x1)
      val (ly, ry) = if (y1 <= y2) (y1, y2) else (y2, y1)
      var res = pref(ry)(rx)
      if (lx > 0) res -= pref(ry)(lx - 1)
      if (ly > 0) res -= pref(ly - 1)(rx)
      if (lx > 0 && ly > 0) res += pref(ly - 1)(lx - 1)
      res
    }

    var maxArea = 0L
    for (i <- points.indices) {
      for (j <- i until points.length) {
        val p1 = points(i)
        val p2 = points(j)
        val area = (abs(p1.x - p2.x).toLong + 1) * (abs(p1.y - p2.y).toLong + 1)
        if (area > maxArea) {
          val (gx1, gy1) = toGrid(p1)
          val (gx2, gy2) = toGrid(p2)
          if (getSum(gx1, gy1, gx2, gy2) == area) maxArea = area
        }
      }
    }

    println(s"Largest valid area: $maxArea")
  }
}
