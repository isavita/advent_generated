
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

case class Point(x: Int, y: Int, z: Int)
case class Edge(u: Int, v: Int, d: Long)

object Main {
  def distSq(a: Point, b: Point): Long = {
    val dx = a.x - b.x
    val dy = a.y - b.y
    val dz = a.z - b.z
    dx.toLong * dx + dy.toLong * dy + dz.toLong * dz
  }

  def find(p: Array[Int], x: Int): Int = {
    var v = x
    while (p(v) != v) {
      p(v) = p(p(v))
      v = p(v)
    }
    v
  }

  def union(p: Array[Int], r: Array[Int], a: Int, b: Int): Unit = {
    var x = a
    var y = b
    if (r(x) < r(y)) p(x) = y
    else if (r(x) > r(y)) p(y) = x
    else { p(y) = x; r(x) += 1 }
  }

  def main(args: Array[String]): Unit = {
    val pts = Source.fromFile("input.txt").getLines()
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap { line =>
        line.split(",") match {
          case Array(a, b, c) =>
            try Some(Point(a.toInt, b.toInt, c.toInt)) catch { case _: NumberFormatException => None }
          case _ => None
        }
      }.toArray

    if (pts.length < 2) return

    val n = pts.length
    val edges = new ArrayBuffer[Edge](n * (n - 1) / 2)
    for (i <- 0 until n; j <- i + 1 until n)
      edges += Edge(i, j, distSq(pts(i), pts(j)))

    val sorted = edges.sortBy(_.d)

    val parent = Array.tabulate(n)(i => i)
    val rank = Array.fill(n)(0)
    var comps = n

    for (e <- sorted if comps > 1) {
      val ru = find(parent, e.u)
      val rv = find(parent, e.v)
      if (ru != rv) {
        union(parent, rank, ru, rv)
        comps -= 1
        if (comps == 1) {
          val p1 = pts(e.u)
          val p2 = pts(e.v)
          println(s"Connected ${p1.x},${p1.y},${p1.z} and ${p2.x},${p2.y},${p2.z}")
          println(s"Product of X coordinates: ${p1.x.toLong * p2.x}")
        }
      }
    }
  }
}
