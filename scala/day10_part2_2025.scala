
import scala.io.Source
import scala.util.matching.Regex
import scala.math.{abs, max, round, sqrt}
import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().filter(_.nonEmpty).toSeq
    var total = 0
    for (line <- lines) {
      val (buttons, targets) = parseLine(line)
      total += solve(buttons, targets)
    }
    println(total)
  }

  def parseLine(line: String): (Array[Array[Int]], Array[Int]) = {
    val btnRe: Regex = """\(([^)]*)\)""".r
    val btns = btnRe.findAllMatchIn(line).map { m =>
      val inside = m.group(1).trim
      if (inside.isEmpty) Array.empty[Int]
      else inside.split(",").map(_.trim.toInt)
    }.toArray

    val tgtRe: Regex = """\{([^}]*)\}""".r
    val targets = tgtRe.findFirstMatchIn(line) match {
      case Some(m) =>
        val inside = m.group(1)
        if (inside.trim.isEmpty) Array.empty[Int]
        else inside.split(",").map(_.trim.toInt)
      case None => Array.empty[Int]
    }
    (btns, targets)
  }

  def solve(buttons: Array[Array[Int]], targets: Array[Int]): Int = {
    val n = targets.length
    val m = buttons.length
    val matrix = Array.ofDim[Double](n, m + 1)
    for (j <- 0 until n) matrix(j)(m) = targets(j).toDouble
    for (i <- buttons.indices; j <- buttons(i) if j < n) matrix(j)(i) = 1.0

    val pivotCol = Array.fill(n)(-1)
    var row = 0
    for (col <- 0 until m if row < n) {
      var maxRow = row
      for (r <- row + 1 until n)
        if (abs(matrix(r)(col)) > abs(matrix(maxRow)(col))) maxRow = r
      if (abs(matrix(maxRow)(col)) < 1e-9) {}
      else {
        val tmp = matrix(row)
        matrix(row) = matrix(maxRow)
        matrix(maxRow) = tmp
        val scale = matrix(row)(col)
        for (c <- col to m) matrix(row)(c) /= scale
        for (r <- 0 until n if r != row && abs(matrix(r)(col)) > 1e-9) {
          val factor = matrix(r)(col)
          for (c <- col to m) matrix(r)(c) -= factor * matrix(row)(c)
        }
        pivotCol(row) = col
        row += 1
      }
    }
    val rank = row
    val isPivot = Array.fill(m)(false)
    val pivotRow = Array.fill(m)(-1)
    for (r <- 0 until rank if pivotCol(r) >= 0) {
      isPivot(pivotCol(r)) = true
      pivotRow(pivotCol(r)) = r
    }
    val freeVars = (0 until m).filterNot(isPivot).toArray

    val maxPresses = Array.fill(m)(Int.MaxValue)
    for (i <- buttons.indices) {
      var limit = Int.MaxValue
      for (j <- buttons(i) if j < n) limit = min(limit, targets(j))
      if (limit == Int.MaxValue) limit = 0
      maxPresses(i) = limit
    }
    scala.util.Sorting.stableSort(freeVars, (a: Int, b: Int) => maxPresses(a) < maxPresses(b))

    var best = Int.MaxValue
    val freeVals = Array.fill(freeVars.length)(0)

    def computePivots(): Option[Array[Int]] = {
      val res = Array.fill(m)(0)
      for (i <- freeVars.indices) res(freeVars(i)) = freeVals(i)
      for (r <- rank - 1 to 0 by -1) {
        val col = pivotCol(r)
        if (col >= 0) {
          var v = matrix(r)(m)
          var c = col + 1
          while (c < m) {
            v -= matrix(r)(c) * res(c)
            c += 1
          }
          val iv = round(v).toInt
          if (abs(v - iv) > 1e-6 || iv < 0 || iv > maxPresses(col)) return None
          res(col) = iv
        }
      }
      Some(res)
    }

    def enumerate(idx: Int, curSum: Int): Unit = {
      if (curSum >= best) return
      if (idx == freeVars.length) {
        computePivots().foreach { arr =>
          val tot = arr.sum
          if (tot < best) best = tot
        }
        return
      }
      val fv = freeVars(idx)
      val limit = maxPresses(fv)
      var v = 0
      while (v <= limit) {
        freeVals(idx) = v
        enumerate(idx + 1, curSum + v)
        v += 1
      }
    }

    enumerate(0, 0)
    if (best == Int.MaxValue) -1 else best
  }

  private def min(a: Int, b: Int): Int = if (a < b) a else b
}
