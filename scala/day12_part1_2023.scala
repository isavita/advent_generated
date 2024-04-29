import scala.io.Source

case class Row(var springs: String, var group: Array[Int])

object Main {
  def parseInput(input: Array[String]): Array[Row] = {
    input.map { line =>
      val parts = line.split(" ")
      val springs = parts(0)
      val group = parts(1).split(",").map(_.toInt)
      Row(springs, group)
    }
  }

  def countArrangementsRecursive(row: Row, iSprings: Int, iGroup: Int, iContiguousDamaged: Int, cache: collection.mutable.Map[(Int, Int, Int), Int]): Int = {
    if (iSprings == row.springs.length) {
      if (iGroup == row.group.length && iContiguousDamaged == 0) 1
      else if (iGroup == row.group.length - 1 && iContiguousDamaged == row.group(iGroup)) 1
      else 0
    } else {
      val cacheKey = (iSprings, iGroup, iContiguousDamaged)
      if (cache.contains(cacheKey)) cache(cacheKey)
      else {
        var res = 0
        val char = row.springs(iSprings)
        if (char == '.' || char == '?') {
          if (iContiguousDamaged == 0) res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged, cache)
          else if (iContiguousDamaged == row.group(iGroup)) res += countArrangementsRecursive(row, iSprings + 1, iGroup + 1, 0, cache)
        }
        if (char == '#' || char == '?') {
          if (iGroup < row.group.length && iContiguousDamaged < row.group(iGroup)) res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged + 1, cache)
        }
        cache(cacheKey) = res
        res
      }
    }
  }

  def countArrangements(row: Row): Int = {
    countArrangementsRecursive(row, 0, 0, 0, collection.mutable.Map())
  }

  def unfoldRow(row: Row, unfoldingFactor: Int): Row = {
    var newRow = row.copy()
    for (_ <- 1 until unfoldingFactor) {
      newRow.springs = newRow.springs + "?" + row.springs
      newRow.group = newRow.group ++ row.group
    }
    newRow
  }

  def solve(input: Array[String]): Int = {
    val rows = parseInput(input)
    var res = 0
    for (row <- rows) res += countArrangements(row)
    res
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toArray
    println(solve(input))
  }
}