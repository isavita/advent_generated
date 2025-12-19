
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

case class Point(id: Int, x: Int, y: Int, z: Int)
case class Edge(u: Int, v: Int, distSq: Long)

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines()
    val points = ArrayBuffer[Point]()
    var id = 0
    for (line <- lines) {
      val t = line.trim
      if (t.nonEmpty) {
        val parts = t.split(",")
        val x = parts(0).toInt
        val y = parts(1).toInt
        val z = parts(2).toInt
        points += Point(id, x, y, z)
        id += 1
      }
    }

    if (points.size < 2) {
      println("Not enough points to form circuits.")
      return
    }

    val edges = ArrayBuffer[Edge]()
    val n = points.size
    for (i <- 0 until n) {
      val p1 = points(i)
      var j = i + 1
      while (j < n) {
        val p2 = points(j)
        val dx = p1.x - p2.x
        val dy = p1.y - p2.y
        val dz = p1.z - p2.z
        val d = dx.toLong * dx + dy.toLong * dy + dz.toLong * dz
        edges += Edge(i, j, d)
        j += 1
      }
    }

    val sortedEdges = edges.sortBy(_.distSq)

    val parent = Array.tabulate(n)(i => i)
    val size   = Array.fill(n)(1)

    def find(i: Int): Int = {
      var x = i
      while (parent(x) != x) {
        parent(x) = parent(parent(x))
        x = parent(x)
      }
      x
    }

    def union(i: Int, j: Int): Unit = {
      var ri = find(i)
      var rj = find(j)
      if (ri != rj) {
        if (size(ri) < size(rj)) { val tmp = ri; ri = rj; rj = tmp }
        parent(rj) = ri
        size(ri) += size(rj)
      }
    }

    val limit = math.min(1000, sortedEdges.length)
    var k = 0
    while (k < limit) {
      val e = sortedEdges(k)
      union(e.u, e.v)
      k += 1
    }

    val circuitSizes = ArrayBuffer[Int]()
    for (i <- 0 until n) if (parent(i) == i) circuitSizes += size(i)

    val top3 = circuitSizes.sorted(Ordering[Int].reverse).take(3)
    val result = top3.foldLeft(1L)(_ * _)

    println(s"Product of three largest circuit sizes: $result")
  }
}
