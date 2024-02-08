object Solution extends App {
  val source = scala.io.Source.fromFile("input.txt")
  val lines = try source.getLines().toList finally source.close()

  val lengths = lines.head.split(",").map(_.toInt)

  var list = (0 until 256).toArray
  var currentPosition = 0
  var skipSize = 0

  for (length <- lengths) {
    for (i <- 0 until length / 2) {
      val start = (currentPosition + i) % 256
      val end = (currentPosition + length - 1 - i) % 256
      val temp = list(start)
      list(start) = list(end)
      list(end) = temp
    }

    currentPosition = (currentPosition + length + skipSize) % 256
    skipSize += 1
  }

  val result = list(0) * list(1)
  println(result)
}