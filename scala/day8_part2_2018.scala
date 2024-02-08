object Main extends App {
  val source = scala.io.Source.fromFile("input.txt")
  val lines = try source.mkString finally source.close()
  val numbers = lines.split(" ").map(_.toInt).toList
  val (value, _) = parseTree(numbers, 0)
  println(value)

  def parseTree(data: List[Int], index: Int): (Int, Int) = {
    val childCount = data(index)
    val metaCount = data(index + 1)
    var currentIndex = index + 2

    val childValues = (1 to childCount).map { _ =>
      val (childValue, newIndex) = parseTree(data, currentIndex)
      currentIndex = newIndex
      childValue
    }

    var value = 0
    if (childCount == 0) {
      value = (0 until metaCount).map(i => data(currentIndex + i)).sum
    } else {
      value = (0 until metaCount).map { i =>
        val metadata = data(currentIndex + i)
        if (metadata <= childCount && metadata > 0) {
          childValues(metadata - 1)
        } else {
          0
        }
      }.sum
    }

    (value, currentIndex + metaCount)
  }
}