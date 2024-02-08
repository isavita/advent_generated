
object Day6 {
  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input.txt").getLines.next.split("\\s+").map(_.toInt).toList
    var configurations = List[List[Int]]()
    var banks = input

    while (!configurations.contains(banks)) {
      configurations = banks :: configurations
      val maxIndex = banks.indexOf(banks.max)
      val blocks = banks(maxIndex)
      banks = banks.updated(maxIndex, 0)
      for (i <- 1 to blocks) {
        banks = banks.updated((maxIndex + i) % banks.length, banks((maxIndex + i) % banks.length) + 1)
      }
    }

    println(configurations.length)
  }
}
