import scala.io.Source
import scala.util.Try

case class Disc(totalPositions: Int, startPosition: Int)

object Main {
  def main(args: Array[String]): Unit = {
    val discs = Source.fromFile("input.txt").getLines().map { line =>
      val regex = """Disc #\d+ has (\d+) positions; at time=0, it is at position (\d+).""".r
      val regex(totalPositions, startPosition) = line
      Disc(totalPositions.toInt, startPosition.toInt)
    }.toSeq :+ Disc(11, 0)

    var time = 0
    while (!checkDiscs(discs, time)) time += 1
    println(time)
  }

  def checkDiscs(discs: Seq[Disc], time: Int): Boolean = discs.zipWithIndex forall { case (disc, i) =>
    (disc.startPosition + time + i + 1) % disc.totalPositions == 0
  }
}