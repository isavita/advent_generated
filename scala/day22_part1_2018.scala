
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList

  val depth = input.head.split(" ").last.toInt
  val target = input.last.split(" ").last.split(",").map(_.toInt)

  val cave = Array.ofDim[Int](target(1) + 1, target(0) + 1)
  val erosion = Array.ofDim[Int](target(1) + 1, target(0) + 1)

  for (y <- 0 to target(1)) {
    for (x <- 0 to target(0)) {
      val geoIndex = if ((x, y) == (0, 0) || (x, y) == (target(0), target(1))) 0
      else if (y == 0) x * 16807
      else if (x == 0) y * 48271
      else erosion(y)(x - 1) * erosion(y - 1)(x)
      erosion(y)(x) = (geoIndex + depth) % 20183
      cave(y)(x) = erosion(y)(x) % 3
    }
  }

  val riskLevel = cave.map(_.sum).sum
  println(riskLevel)
}
