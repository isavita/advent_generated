object Solution extends App {
  import scala.io.Source

  case class Disc(totalPositions: Int, startPosition: Int)

  val discs = Source.fromFile("input.txt").getLines.map { line =>
    val pattern = "Disc #(\\d+) has (\\d+) positions; at time=0, it is at position (\\d+).".r
    val pattern(discNum, totalPositions, startPosition) = line
    Disc(totalPositions.toInt, startPosition.toInt)
  }.toList

  def checkDiscs(discs: List[Disc], time: Int): Boolean = {
    discs.zipWithIndex.forall { case (disc, i) =>
      val position = (disc.startPosition + time + i + 1) % disc.totalPositions
      position == 0
    }
  }

  var time = 0
  while (!checkDiscs(discs, time)) {
    time += 1
  }

  println(time)
}