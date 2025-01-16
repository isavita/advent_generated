
import scala.io.Source
import scala.collection.mutable

object Main extends App {
  val input = Source.fromFile("input.txt").getLines().toArray
  val graph = input.map(_.toCharArray).toArray
  val H = graph.length
  val W = graph(0).length

  val moves = Array(
    ("left", -1, 0),
    ("up", 0, -1),
    ("right", 1, 0),
    ("down", 0, 1)
  )

  var sum = 0

  for {
    y <- 0 until H
    x <- 0 until W
    if graph(y)(x) != '.'
  } {
    var area = 0
    val target = graph(y)(x)
    val visited = mutable.Set.empty[(Int, Int)]
    val side = mutable.Map.empty[String, mutable.Set[(Int, Int)]]

    def search(cx: Int, cy: Int, label: String): Unit = {
      if (graph(cy)(cx) != target) {
        if (label.nonEmpty && !visited((cx, cy))) saveOuter(label, side, cx, cy)
        return
      }

      visited += ((cx, cy))
      area += 1
      graph(cy)(cx) = '.'

      for ((label, dx, dy) <- moves) {
        val nx = cx + dx
        val ny = cy + dy

        if (nx < 0 || nx >= W || ny < 0 || ny >= H) {
          saveOuter(label, side, nx, ny)
        } else {
          search(nx, ny, label)
        }
      }
    }

    search(x, y, "")
    val outer = countOuter(side)
    sum += area * outer
  }

  println(sum)

  def saveOuter(label: String, side: mutable.Map[String, mutable.Set[(Int, Int)]], x: Int, y: Int): Unit = {
    val key = if (label == "up" || label == "down") (y, x) else (x, y)
    side.getOrElseUpdate(label, mutable.Set.empty) += key
  }

  def countOuter(side: mutable.Map[String, mutable.Set[(Int, Int)]]): Int = {
    var outer = 0
    for (label <- side.keys) {
      val array = side(label).toArray.sortBy { case (i, j) => (i, j) }
      var temp = mutable.Set.empty[(Int, Int)]
      for ((i, j) <- array) {
        if (!check(temp, i, j)) outer += 1
        temp += ((i, j))
      }
    }
    outer
  }

  def check(ary: mutable.Set[(Int, Int)], i: Int, j: Int): Boolean = {
    val search = Array((i, j - 1), (i, j + 1))
    search.exists(ary.contains)
  }
}
