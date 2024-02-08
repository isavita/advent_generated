object Main extends App {
  val totalElves: Int = io.Source.fromFile("input.txt").getLines().next().toInt
  val winner: Int = findWinningElf(totalElves)
  println(winner)

  def findWinningElf(totalElves: Int): Int = {
    var highestPowerOfTwo = 1
    while (highestPowerOfTwo * 2 <= totalElves) {
      highestPowerOfTwo *= 2
    }
    (totalElves - highestPowerOfTwo) * 2 + 1
  }
}