
import scala.io.Source

object DiskFragmenter {

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString.trim
    val disk = Disk(input)
    disk.compact()
    val checksum = disk.calculateChecksum()
    println(checksum)
  }

  case class Disk(layout: String) {

    private val segments: Vector[(Int, Int)] = {
      val lengths = layout.map(_.asDigit).toVector
      var i = 0
      var fileId = 0
      var result = Vector.empty[(Int, Int)]
      while (i < lengths.length) {
        val fileLength = lengths(i)
        i += 1
        if (i < lengths.length) {
          val freeLength = lengths(i)
          result = result :+ (fileLength, fileId) :+ (freeLength, -1)  //-1 represents free space
          fileId += 1
          i += 1
        } else {
          result = result :+ (fileLength, fileId)
        }
      }
      result
    }
   
    private var diskState: Vector[Int] = segments.flatMap { case (len, id) => Vector.fill(len)(id) }

    def compact(): Unit = {
      var freeIndex = 0
      var fileIndex = diskState.length - 1

      while (freeIndex < diskState.length && fileIndex >= 0) {
        if (diskState(freeIndex) != -1) {
          freeIndex += 1
        } else if (diskState(fileIndex) == -1) {
          fileIndex -= 1
        } else if (freeIndex < fileIndex) {
          diskState = diskState.updated(freeIndex, diskState(fileIndex))
          diskState = diskState.updated(fileIndex, -1)
          freeIndex += 1
          fileIndex -= 1
        } else {
          return
        }
      }
    }


    def calculateChecksum(): Long = {
      diskState.zipWithIndex.foldLeft(0L) { (acc, elem) =>
        val (fileId, index) = elem
        if (fileId != -1) {
          acc + (index.toLong * fileId.toLong)
        } else {
          acc
        }
      }
    }

    override def toString: String = {
      diskState.map(id => if(id == -1) "." else id.toString).mkString
    }
  }
}
