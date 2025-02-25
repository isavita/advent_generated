
import scala.io.Source

object Solution {

  case class FileSegment(id: Int, start: Int, end: Int)

  def solve(): Unit = {
    val line = Source.fromFile("input.txt").getLines().next().trim

    var disk: Array[String] = Array.empty[String]
    var fileId = 0
    var isFile = true

    for (char <- line) {
      val length = char.asDigit
      if (isFile) {
        disk = disk ++ Array.fill(length)(fileId.toString)
        fileId += 1
      } else {
        disk = disk ++ Array.fill(length)(".")
      }
      isFile = !isFile
    }

    var files: List[FileSegment] = List.empty[FileSegment]
    var currId: Option[Int] = None
    var start = 0

    for ((valAtIdx, i) <- disk.zipWithIndex) {
      if (valAtIdx == ".") {
        currId = None
      } else {
        val fileId = valAtIdx.toInt
        if (currId.isEmpty || fileId != currId.get) {
          currId = Some(fileId)
          start = i
        }

        if (i == disk.length - 1 || (i + 1 < disk.length && disk(i + 1) != valAtIdx)) {
          files = files :+ FileSegment(fileId, start, i)
        }
      }
    }

    for (file <- files.reverse) {
      val fileLen = file.end - file.start + 1
      var leftmostSpan = -1
      var spanLen = 0

      var foundSpan = false
      for (i <- 0 until file.start if !foundSpan) {
        if (disk(i) == ".") {
          if (spanLen == 0) {
            leftmostSpan = i
          }
          spanLen += 1
          if (spanLen == fileLen) {
            foundSpan = true
          }
        } else {
          spanLen = 0
          leftmostSpan = -1
        }
      }

      if (leftmostSpan != -1 && spanLen == fileLen) {
        for (i <- file.start to file.end) {
          disk(i) = "."
        }
        for (i <- 0 until fileLen) {
          disk(leftmostSpan + i) = file.id.toString
        }
      }
    }

    var checksum: Long = 0
    for ((valAtIdx, i) <- disk.zipWithIndex) {
      if (valAtIdx != ".") {
        checksum += i * valAtIdx.toInt
      }
    }
    println(checksum)
  }

  def main(args: Array[String]): Unit = {
    solve()
  }
}
