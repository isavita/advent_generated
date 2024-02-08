object Main extends App {
  val source = scala.io.Source.fromFile("input.txt")
  val lines = try source.getLines().toList finally source.close()

  var sumOfSectorIDs = 0
  for (line <- lines) {
    if (isRealRoom(line)) {
      sumOfSectorIDs += getSectorID(line)
    }
  }

  println(sumOfSectorIDs)

  def isRealRoom(room: String): Boolean = {
    val parts = room.split("\\[")
    val checksum = parts(1).stripSuffix("]")
    var encryptedName = parts(0).split("-")
    encryptedName = encryptedName.dropRight(1)

    var letterCounts = scala.collection.mutable.Map[Char, Int]().withDefaultValue(0)
    for (part <- encryptedName) {
      for (letter <- part) {
        letterCounts(letter) += 1
      }
    }

    val counts = letterCounts.toList.sortWith {
      case ((letter1, count1), (letter2, count2)) =>
        if (count1 == count2) letter1 < letter2 else count1 > count2
    }

    for (i <- checksum.indices) {
      if (checksum(i) != counts(i)._1) {
        return false
      }
    }

    true
  }

  def getSectorID(room: String): Int = {
    val parts = room.split("-")
    val sectorIDPart = parts.last
    sectorIDPart.split("\\[")(0).toInt
  }
}