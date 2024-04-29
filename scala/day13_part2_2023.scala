object Main {
  case class Mirror(rows: Array[Int], cols: Array[Int])

  def parseInput(input: Array[String]): Array[Mirror] = {
    val mirrors = scala.collection.mutable.ArrayBuffer[Mirror]()
    var mirrorStr = scala.collection.mutable.ArrayBuffer[String]()

    for (line <- input) {
      if (line.isEmpty) {
        mirrors += parseMirror(mirrorStr.toArray)
        mirrorStr.clear()
      } else {
        mirrorStr += line
      }
    }
    mirrors += parseMirror(mirrorStr.toArray)

    mirrors.toArray
  }

  def parseMirror(mirrorStr: Array[String]): Mirror = {
    val rows = new Array[Int](mirrorStr.length)
    val cols = new Array[Int](mirrorStr(0).length)

    for (y <- mirrorStr.indices; x <- mirrorStr(y).indices) {
      rows(y) <<= 1
      cols(x) <<= 1
      if (mirrorStr(y)(x) == '#') {
        rows(y) += 1
        cols(x) += 1
      }
    }

    Mirror(rows, cols)
  }

  def getMirrorAxis(lines: Array[Int]): Int = {
    for (i <- 1 until lines.length) {
      var isMirror = true

      for (j <- 0 until math.min(i, lines.length - i)) {
        if (lines(i - 1 - j) != lines(i + j)) {
          isMirror = false
        }
      }

      if (isMirror) {
        return i
      }
    }

    0
  }

  def getMirrorAxisWithOneSmudge(lines: Array[Int]): Int = {
    for (i <- 1 until lines.length) {
      var isMirror = true
      var numSmudges = 0

      for (j <- 0 until math.min(i, lines.length - i)) {
        if (lines(i - 1 - j) != lines(i + j)) {
          if (numSmudges > 0) {
            isMirror = false
          } else {
            val dif = lines(i - 1 - j) ^ lines(i + j)
            val isOnlyOneSmudge = (dif & (dif - 1)) == 0
            if (isOnlyOneSmudge) {
              numSmudges += 1
            } else {
              isMirror = false
            }
          }
        }
      }

      if (isMirror && numSmudges == 1) {
        return i
      }
    }

    0
  }

  def solve(input: Array[String]): Int = {
    val mirrors = parseInput(input)

    var res = 0
    for (mirror <- mirrors) {
      res += getMirrorAxisWithOneSmudge(mirror.cols)
      res += getMirrorAxisWithOneSmudge(mirror.rows) * 100
    }
    res
  }

  def readFile(fileName: String): Array[String] = {
    val file = scala.io.Source.fromFile(fileName)
    val lines = file.getLines().toArray
    file.close()
    lines
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("input.txt")
    println(solve(input))
  }
}