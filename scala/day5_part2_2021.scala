import scala.io.Source

object Main extends App {
  val lines = Source.fromFile("input.txt").getLines().toArray
  val overlaps = scala.collection.mutable.Map[(Int, Int), Int]()

  for (line <- lines) {
    val parts = line.split(" -> ")
    val start = parts(0).split(",")
    val end = parts(1).split(",")

    val x1 = start(0).toInt
    val y1 = start(1).toInt
    val x2 = end(0).toInt
    val y2 = end(1).toInt

    val xStep = sign(x2 - x1)
    val yStep = sign(y2 - y1)
    var steps = abs(x2 - x1) + 1
    if (math.abs(y2 - y1) > math.abs(x2 - x1)) {
      steps = math.abs(y2 - y1) + 1
    }

    for (i <- 0 until steps) {
      val point = (x1 + i * xStep, y1 + i * yStep)
      if (overlaps.contains(point)) {
        overlaps(point) += 1
      } else {
        overlaps(point) = 1
      }
    }
  }

  var count = 0
  for ((_, v) <- overlaps) {
    if (v > 1) {
      count += 1
    }
  }

  println(count)

  def abs(x: Int): Int = if (x < 0) -x else x

  def sign(x: Int): Int = if (x > 0) 1 else if (x < 0) -1 else 0
}