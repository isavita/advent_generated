import scala.io.Source
import scala.util.Try

object Main extends App {
  val values = Source.fromFile("input.txt").getLines().toArray

  def filterValues(values: Array[String], criteria: (Int, Int) => Char): String = {
    var valuesLeft = values
    for (i <- values.head.indices) {
      val (zeros, ones) = valuesLeft.foldLeft((0, 0)) { case ((zeros, ones), valStr) =>
        if (valStr(i) == '0') (zeros + 1, ones) else (zeros, ones + 1)
      }
      val keep = criteria(zeros, ones)
      valuesLeft = valuesLeft.filter(_(i) == keep)
      if (valuesLeft.length == 1) return valuesLeft.head
    }
    valuesLeft.head
  }

  val oxygenGeneratorRating = filterValues(values, (zeros, ones) => if (zeros > ones) '0' else '1')
  val co2ScrubberRating = filterValues(values, (zeros, ones) => if (zeros <= ones) '0' else '1')

  val oxygenGeneratorRatingInt = BigInt(oxygenGeneratorRating, 2).toInt
  val co2ScrubberRatingInt = BigInt(co2ScrubberRating, 2).toInt

  println(oxygenGeneratorRatingInt * co2ScrubberRatingInt)
}