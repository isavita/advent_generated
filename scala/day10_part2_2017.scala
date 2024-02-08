
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.next

  def knotHash(input: String): String = {
    val lengths = input.map(_.toInt).mkString(",") + ",17,31,73,47,23"
    val sparseHash = (0 to 255).toArray
    var currentPosition = 0
    var skipSize = 0

    def reverse(list: Array[Int], start: Int, length: Int): Unit = {
      val end = (start + length - 1) % list.length
      var i = 0
      while (i < length / 2) {
        val temp = list((start + i) % list.length)
        list((start + i) % list.length) = list((end - i + list.length) % list.length)
        list((end - i + list.length) % list.length) = temp
        i += 1
      }
    }

    for (_ <- 1 to 64) {
      for (length <- lengths.split(",").map(_.toInt)) {
        reverse(sparseHash, currentPosition, length)
        currentPosition = (currentPosition + length + skipSize) % sparseHash.length
        skipSize += 1
      }
    }

    val denseHash = sparseHash.grouped(16).map(_.reduce(_ ^ _)).map(_.formatted("%02x")).mkString
    denseHash
  }

  println(knotHash(input))
}
