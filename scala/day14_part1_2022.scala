
import java.awt.Point
import java.io.File
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val grid = collection.mutable.Set[Point]()
    val lines = Source.fromFile("input.txt").getLines()
    for (line <- lines) {
      val parts = line.split(" -> ")
      val points = parts.map { p =>
        val Array(x, y) = p.split(",").map(_.toInt)
        new Point(x, y)
      }
      for (i <- 0 until points.length - 1) {
        val p1 = points(i)
        val p2 = points(i + 1)
        if (p1.x == p2.x) {
          for (y <- math.min(p1.y, p2.y) to math.max(p1.y, p2.y)) {
            grid.add(new Point(p1.x, y))
          }
        } else {
          for (x <- math.min(p1.x, p2.x) to math.max(p1.x, p2.x)) {
            grid.add(new Point(x, p1.y))
          }
        }
      }
    }
    println(fill(grid))
  }

  def fill(grid: collection.mutable.Set[Point]): Int = {
    val floor = grid.map(_.y).max + 2
    var sands = 0
    var firstFloorTouch = 0
    while (!grid.contains(new Point(500, 0))) {
      var sand = new Point(500, 0)
      var settled = false
      while (!settled) {
        val nextPos = next(grid, sand, floor)
        if (nextPos == sand) {
          settled = true
        } else {
          sand = nextPos
        }
        if (sand.y == floor) {
          if (firstFloorTouch == 0) {
            firstFloorTouch = sands
          }
          grid.add(sand)
          settled = true
        }
      }
      sands += 1
    }
    firstFloorTouch
  }

  val D = new Point(0, 1)
  val L = new Point(-1, 0)
  val R = new Point(1, 0)

  def next(grid: collection.mutable.Set[Point], sand: Point, floor: Int): Point = {
    val nextPositions = List(
      new Point(sand.x + D.x, sand.y + D.y),
      new Point(sand.x + D.x + L.x, sand.y + D.y + L.y),
      new Point(sand.x + D.x + R.x, sand.y + D.y + R.y)
    )
    for (n <- nextPositions) {
      if (!grid.contains(n)) {
        return n
      }
    }
    grid.add(sand)
    sand
  }
}
