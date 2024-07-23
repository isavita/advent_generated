
import scala.io.Source
import scala.collection.mutable
import scala.util.Sorting

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines()
    for (line <- lines) {
      if (isRealRoom(line)) {
        val decryptedName = decryptName(line)
        if (decryptedName.contains("northpole object")) {
          println(getSectorID(line))
          return
        }
      }
    }
  }

  def isRealRoom(room: String): Boolean = {
    val Array(encryptedPart, checksum) = room.split("\\[")
    val cleanChecksum = checksum.dropRight(1)
    val encryptedName = encryptedPart.split("-").dropRight(1)

    val letterCounts = mutable.Map[Char, Int]().withDefaultValue(0)
    encryptedName.foreach(_.foreach(letter => letterCounts(letter) += 1))

    val counts = letterCounts.toSeq.sortBy { case (letter, count) => (-count, letter) }
    counts.take(5).map(_._1).mkString("") == cleanChecksum
  }

  def getSectorID(room: String): Int = {
    room.split("-").last.split("\\[")(0).toInt
  }

  def decryptName(room: String): String = {
    val sectorID = getSectorID(room)
    val encryptedParts = room.split("-").dropRight(1)

    encryptedParts.map(_.map {
      case '-' => ' '
      case letter => ((letter - 'a' + sectorID) % 26 + 'a').toChar
    }).mkString(" ")
  }
}
