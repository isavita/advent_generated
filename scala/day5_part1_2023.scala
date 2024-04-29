import scala.io.Source
import scala.util.Try

case class RangeMap(srcStart: Long, destStart: Long, length: Long)

object Main extends App {
  val file = Source.fromFile("input.txt")
  val lines = file.getLines().toList
  file.close()

  var seeds: List[Long] = Nil
  var currentRanges: List[RangeMap] = Nil
  var maps: List[List[RangeMap]] = Nil

  for (line <- lines) {
    if (line.contains("map:")) {
      if (currentRanges.nonEmpty) {
        maps = maps :+ currentRanges
        currentRanges = Nil
      }
    } else if (line.startsWith("seeds:")) {
      val seedStrs = line.substring(7).split(" ")
      seeds = seedStrs.flatMap(s => Try(s.toLong).toOption).toList
    } else {
      val numbers = line.split("\\s+")
      if (numbers.length == 3) {
        val srcStart = numbers(1).toLong
        val destStart = numbers(0).toLong
        val length = numbers(2).toLong
        currentRanges = RangeMap(srcStart, destStart, length) :: currentRanges
      }
    }
  }
  maps = maps :+ currentRanges

  def convertNumber(number: Long, ranges: List[RangeMap]): Long = {
    ranges.find(r => number >= r.srcStart && number < r.srcStart + r.length) match {
      case Some(r) => r.destStart + (number - r.srcStart)
      case None => number
    }
  }

  var minLocation = Long.MaxValue
  for (seed <- seeds) {
    var location = seed
    for (m <- maps) {
      location = convertNumber(location, m)
    }
    if (location < minLocation) {
      minLocation = location
    }
  }

  println(minLocation)
}