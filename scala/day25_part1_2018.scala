
import scala.io.Source

case class Point(x: Int, y: Int, z: Int, t: Int) {
  def manhattanDistance(other: Point): Int =
    Math.abs(x - other.x) + Math.abs(y - other.y) + Math.abs(z - other.z) + Math.abs(t - other.t)
}

class UnionFind(size: Int) {
  private val parent = Array.tabulate(size)(identity)

  def find(x: Int): Int = {
    if (parent(x) != x) parent(x) = find(parent(x))
    parent(x)
  }

  def union(x: Int, y: Int): Unit = {
    val rootX = find(x)
    val rootY = find(y)
    if (rootX != rootY) parent(rootX) = rootY
  }

  def countGroups: Int = parent.indices.count(i => i == find(i))
}

object Main extends App {
  val points = Source.fromFile("input.txt").getLines().toArray.map { line =>
    val coords = line.split(",").map(_.toInt)
    Point(coords(0), coords(1), coords(2), coords(3))
  }

  val uf = new UnionFind(points.length)
  for (i <- points.indices; j <- points.indices if i != j) {
    if (points(i).manhattanDistance(points(j)) <= 3) {
      uf.union(i, j)
    }
  }

  println(uf.countGroups)
}
