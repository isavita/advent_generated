
import scala.io.Source
import scala.collection.mutable

object Main {
  case class Row(springs: String, group: List[Int])

  def parseInput(input: List[String]): List[Row] = {
    input.map { line =>
      val parts = line.split(" ")
      val springs = parts(0)
      val ints = parts(1).split(",").map(_.toInt).toList
      Row(springs, ints)
    }
  }

  def countArrangementsRecursive(row: Row, iSprings: Int, iGroup: Int, iContiguousDamaged: Int, cache: mutable.Map[(Int, Int, Int), Long]): Long = {
    if (iSprings == row.springs.length) {
      if (iGroup == row.group.length && iContiguousDamaged == 0) 1
      else if (iGroup == row.group.length - 1 && iContiguousDamaged == row.group(iGroup)) 1
      else 0
    } else {
      val cacheKey = (iSprings, iGroup, iContiguousDamaged)
      cache.get(cacheKey) match {
        case Some(value) => value
        case None =>
          var res: Long = 0
          val char = row.springs(iSprings)
          if (char == '.' || char == '?') {
            if (iContiguousDamaged == 0) {
              res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged, cache)
            } else if (iContiguousDamaged == row.group(iGroup)) {
              res += countArrangementsRecursive(row, iSprings + 1, iGroup + 1, 0, cache)
            }
          }
          if (char == '#' || char == '?') {
            if (iGroup < row.group.length && iContiguousDamaged < row.group(iGroup)) {
              res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged + 1, cache)
            }
          }
          cache.put(cacheKey, res)
          res
      }
    }
  }

  def countArrangements(row: Row): Long = {
    countArrangementsRecursive(row, 0, 0, 0, mutable.Map[(Int, Int, Int), Long]())
  }

  def unfoldRow(row: Row, unfoldingFactor: Int): Row = {
    val newSprings = List.fill(unfoldingFactor)(row.springs).mkString("?")
    val newGroup = List.fill(unfoldingFactor)(row.group).flatten
    Row(newSprings, newGroup)
  }

  def solve(input: List[String]): Long = {
    val rows = parseInput(input)
    val unfoldedRows = rows.map(row => unfoldRow(row, 5))
    unfoldedRows.map(countArrangements).sum
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    println(solve(input))
  }
}
