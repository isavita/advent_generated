
import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.io.Source

case class Point(r: Int, c: Int)
case class Piece(p: Array[Point]) {
  val n: Int = p.length
}
object Main {
  def trim(s: String): String = s.trim
  def cmp(a: Point, b: Point): Boolean =
    if (a.r != b.r) a.r < b.r else a.c < b.c
  def normalize(p: Piece): Piece = {
    if (p.n == 0) return p
    val minR = p.p.minBy(_.r).r
    val minC = p.p.minBy(_.c).c
    val arr = p.p.map(pt => Point(pt.r - minR, pt.c - minC)).sortWith(cmp)
    Piece(arr)
  }
  def rotate(p: Piece): Piece = {
    val arr = p.p.map(pt => Point(pt.c, -pt.r))
    Piece(arr)
  }
  def flip(p: Piece): Piece = {
    val arr = p.p.map(pt => Point(pt.r, -pt.c))
    Piece(arr)
  }
  def equal(a: Piece, b: Piece): Boolean =
    a.n == b.n && a.p.sameElements(b.p)
  def generateVariations(base: Piece): Array[Piece] = {
    val uniq = mutable.ArrayBuffer.empty[Piece]
    var cur = base
    for (_ <- 0 until 4) {
      val n = normalize(cur)
      if (!uniq.exists(equal(_, n))) uniq += n
      val f = flip(cur)
      val nf = normalize(f)
      if (!uniq.exists(equal(_, nf))) uniq += nf
      cur = rotate(cur)
    }
    uniq.toArray
  }
  def canPlace(rows: Int, cols: Int, grid: Array[Byte], p: Piece, r: Int, c: Int): Boolean = {
    var i = 0
    while (i < p.n) {
      val nr = r + p.p(i).r
      val nc = c + p.p(i).c
      if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) return false
      if (grid(nr * cols + nc) != 0) return false
      i += 1
    }
    true
  }
  def place(cols: Int, grid: Array[Byte], p: Piece, r: Int, c: Int, v: Byte): Unit = {
    var i = 0
    while (i < p.n) {
      grid((r + p.p(i).r) * cols + (c + p.p(i).c)) = v
      i += 1
    }
  }
  def checkIslands(rows: Int, cols: Int, grid: Array[Byte], counts: Array[Int],
                   arrSize: Int, slackIdx: Int, shapes: Array[Piece]): Boolean = {
    var minReal = Int.MaxValue
    var hasReal = false
    var i = 0
    while (i < arrSize) {
      if (i != slackIdx && counts(i) > 0) {
        if (shapes(i).n < minReal) minReal = shapes(i).n
        hasReal = true
      }
      i += 1
    }
    if (!hasReal) return true
    var avail = counts(slackIdx)
    val vis = new Array[Boolean](rows * cols)
    val q = new Array[Int](rows * cols)
    var idx = 0
    while (idx < rows * cols) {
      if (grid(idx) == 0 && !vis(idx)) {
        var qs = 0
        var qe = 0
        q(qe) = idx; qe += 1; vis(idx) = true
        var size = 0
        while (qs < qe) {
          val cur = q(qs); qs += 1; size += 1
          val r = cur / cols
          val c = cur % cols
          if (r > 0) {
            val n = (r - 1) * cols + c
            if (grid(n) == 0 && !vis(n)) { vis(n) = true; q(qe) = n; qe += 1 }
          }
          if (r < rows - 1) {
            val n = (r + 1) * cols + c
            if (grid(n) == 0 && !vis(n)) { vis(n) = true; q(qe) = n; qe += 1 }
          }
          if (c > 0) {
            val n = r * cols + (c - 1)
            if (grid(n) == 0 && !vis(n)) { vis(n) = true; q(qe) = n; qe += 1 }
          }
          if (c < cols - 1) {
            val n = r * cols + (c + 1)
            if (grid(n) == 0 && !vis(n)) { vis(n) = true; q(qe) = n; qe += 1 }
          }
        }
        if (size < minReal) {
          if (avail >= size) avail -= size else return false
        }
      }
      idx += 1
    }
    true
  }
  def solveRec(rows: Int, cols: Int, grid: Array[Byte], counts: Array[Int],
               arrSize: Int, ids: Array[Int], idCount: Int,
               variations: Array[Array[Piece]], varCounts: Array[Int],
               slackIdx: Int, shapes: Array[Piece]): Boolean = {
    var empty = -1
    var i = 0
    while (i < rows * cols && empty == -1) {
      if (grid(i) == 0) empty = i
      i += 1
    }
    if (empty == -1) return true
    val r = empty / cols
    val c = empty % cols
    if (!checkIslands(rows, cols, grid, counts, arrSize, slackIdx, shapes)) return false
    var ii = 0
    while (ii < idCount) {
      val id = ids(ii)
      if (counts(id) > 0) {
        counts(id) -= 1
        var v = 0
        while (v < varCounts(id)) {
          val p = variations(id)(v)
          if (canPlace(rows, cols, grid, p, r, c)) {
            place(cols, grid, p, r, c, 1)
            if (solveRec(rows, cols, grid, counts, arrSize, ids, idCount,
                         variations, varCounts, slackIdx, shapes)) return true
            place(cols, grid, p, r, c, 0)
          }
          v += 1
        }
        counts(id) += 1
      }
      ii += 1
    }
    false
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toArray
    var maxId = -1000000
    lines.foreach { l =>
      val s = trim(l)
      if (s.nonEmpty && s.last == ':') {
        val id = s.dropRight(1).toInt
        if (id > maxId) maxId = id
      }
    }
    if (maxId < 0) maxId = -1
    val arrSize = maxId + 2
    val slackIdx = maxId + 1

    val shapes = Array.fill[Piece](arrSize)(Piece(Array.empty))
    var parsingShapes = true
    var currentId = -1
    var curShape = mutable.ArrayBuffer.empty[String]
    val regionLines = mutable.ArrayBuffer.empty[String]

    lines.foreach { raw =>
      val s = trim(raw)
      if (s.isEmpty) ()
      else {
        if (s.contains('x') && s.contains(':')) parsingShapes = false
        if (parsingShapes) {
          if (s.last == ':') {
            if (currentId != -1 && curShape.nonEmpty) {
              val pts = mutable.ArrayBuffer.empty[Point]
              for (r <- curShape.indices; c <- 0 until curShape(r).length)
                if (curShape(r)(c) == '#') pts += Point(r, c)
              shapes(currentId) = normalize(Piece(pts.toArray))
              curShape.clear()
            }
            currentId = s.dropRight(1).toInt
          } else {
            curShape += s
          }
        } else {
          regionLines += s
        }
      }
    }
    if (currentId != -1 && curShape.nonEmpty) {
      val pts = mutable.ArrayBuffer.empty[Point]
      for (r <- curShape.indices; c <- 0 until curShape(r).length)
        if (curShape(r)(c) == '#') pts += Point(r, c)
      shapes(currentId) = normalize(Piece(pts.toArray))
    }

    shapes(slackIdx) = Piece(Array(Point(0, 0)))

    val variations = Array.ofDim[Array[Piece]](arrSize)
    val varCounts = new Array[Int](arrSize)
    for (i <- 0 until arrSize) {
      if (shapes(i).n > 0) {
        val vars = generateVariations(shapes(i))
        variations(i) = vars
        varCounts(i) = vars.length
      } else {
        variations(i) = Array.empty
      }
    }

    var solved = 0
    regionLines.foreach { line =>
      val parts = line.split(":")
      if (parts.length == 2) {
        val dims = parts(0).trim
        val countsStr = parts(1).trim
        val dimParts = dims.split("x")
        if (dimParts.length == 2) {
          val wx = dimParts(0).toInt
          val h = dimParts(1).toInt
          val gridSize = wx * h
          val pieceCounts = new Array[Int](arrSize)
          var totalArea = 0
          val toks = countsStr.split("\\s+")
          var idx = 0
          while (idx < toks.length && idx < arrSize - 1) {
            val c = toks(idx).toInt
            if (c > 0) {
              pieceCounts(idx) = c
              totalArea += c * shapes(idx).n
            }
            idx += 1
          }
          if (totalArea <= gridSize) {
            val slack = gridSize - totalArea
            if (slack > 0) pieceCounts(slackIdx) = slack
            val ids = (0 until arrSize).filter(pieceCounts(_) > 0).toArray
            val sortedIds = ids.sortWith((a, b) => shapes(a).n > shapes(b).n)
            val grid = new Array[Byte](gridSize)
            if (solveRec(h, wx, grid, pieceCounts, arrSize, sortedIds, sortedIds.length,
                         variations, varCounts, slackIdx, shapes)) solved += 1
          }
        }
      }
    }

    println(s"Number of regions that fit all presents: $solved")
  }
}
