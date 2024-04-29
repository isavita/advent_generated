object MemoryReallocation {
  def main(args: Array[String]): Unit = {
    val input = io.Source.fromFile("input.txt").getLines.next.split("\\s+").map(_.toInt)
    val seen = scala.collection.mutable.Map[String, Int]()
    var banks = input
    var steps = 0
    var loopSize = 0

    while (!seen.contains(banks.mkString(","))) {
      seen(banks.mkString(",")) = steps
      val maxBlocks = banks.max
      val maxIndex = banks.indexOf(maxBlocks)
      banks = banks.updated(maxIndex, 0)
      var index = (maxIndex + 1) % banks.length
      for (_ <- 1 to maxBlocks) {
        banks = banks.updated(index, banks(index) + 1)
        index = (index + 1) % banks.length
      }
      steps += 1
    }

    loopSize = steps - seen(banks.mkString(","))
    println("Part 1: " + steps)
    println("Part 2: " + loopSize)
  }
}