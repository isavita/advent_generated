
object Day3 extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.toList

  val claims = input.map { line =>
    val parts = line.split(" ")
    val id = parts(0).substring(1).toInt
    val coords = parts(2).init.split(",").map(_.toInt)
    val size = parts(3).split("x").map(_.toInt)
    (id, coords(0), coords(1), size(0), size(1))
  }

  val fabric = Array.ofDim[Int](1000, 1000)
  var count = 0

  for ((id, x, y, w, h) <- claims) {
    for (i <- x until x + w; j <- y until y + h) {
      if (fabric(i)(j) == 1) count += 1
      fabric(i)(j) += 1
    }
  }

  println(count)

  val nonOverlapping = claims.find { case (id, x, y, w, h) =>
    var overlapping = false
    for (i <- x until x + w; j <- y until y + h) {
      if (fabric(i)(j) > 1) overlapping = true
    }
    !overlapping
  }

  println(nonOverlapping.get._1)
}
