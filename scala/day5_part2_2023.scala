
import scala.io.Source

object Day5 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString
    val parts = input.split("\n\n")
    
    val seeds = parts.head.split(": ")(1).split(" ").map(_.toLong)
    
    val maps = parts.tail.map { section =>
      section.split("\n").tail.map { line =>
        val Array(dest, src, len) = line.split(" ").map(_.toLong)
        (src, src + len - 1, dest - src)
      }
    }
    
    // Part 1
    val lowestLocation1 = seeds.map(seedToLocation(_, maps)).min
    println(s"Part 1: $lowestLocation1")
    
    // Part 2
    val seedRanges = seeds.grouped(2).map(arr => (arr(0), arr(0) + arr(1) - 1)).toList
    val lowestLocation2 = findLowestLocation(seedRanges, maps)
    println(s"Part 2: $lowestLocation2")
  }
  
  def seedToLocation(seed: Long, maps: Array[Array[(Long, Long, Long)]]): Long = {
    maps.foldLeft(seed) { (value, map) =>
      map.find(range => value >= range._1 && value <= range._2)
         .map(range => value + range._3)
         .getOrElse(value)
    }
  }
  
  def findLowestLocation(seedRanges: List[(Long, Long)], maps: Array[Array[(Long, Long, Long)]]): Long = {
    maps.foldLeft(seedRanges) { (ranges, map) =>
      ranges.flatMap { case (start, end) =>
        mapRange(start, end, map)
      }
    }.map(_._1).min
  }
  
  def mapRange(start: Long, end: Long, map: Array[(Long, Long, Long)]): List[(Long, Long)] = {
    val mappedRanges = map.flatMap { case (srcStart, srcEnd, offset) =>
      val overlapStart = math.max(start, srcStart)
      val overlapEnd = math.min(end, srcEnd)
      
      if (overlapStart <= overlapEnd) {
        Some((overlapStart + offset, overlapEnd + offset))
      } else None
    }
    
    if (mappedRanges.isEmpty) List((start, end))
    else {
      val sortedRanges = mappedRanges.sortBy(_._1)
      val mergedRanges = sortedRanges.foldLeft(List.empty[(Long, Long)]) { (acc, range) =>
        acc match {
          case Nil => List(range)
          case last :: rest if last._2 + 1 >= range._1 =>
            (last._1, math.max(last._2, range._2)) :: rest
          case _ => range :: acc
        }
      }.reverse
      
      mergedRanges
    }
  }
}
