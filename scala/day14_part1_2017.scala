
import scala.io.Source

object Solution {
  def reverseSection(arr: Array[Int], start: Int, length: Int): Unit = {
    val n = arr.length
    for (i <- 0 until length / 2) {
      val temp = arr((start + i) % n)
      arr((start + i) % n) = arr((start + length - i - 1) % n)
      arr((start + length - i - 1) % n) = temp
    }
  }

  def knotHash(input: String): String = {
    var lengths = input.map(_.toInt).toList
    lengths ++= List(17, 31, 73, 47, 23)

    var list = (0 until 256).toArray

    var position = 0
    var skip = 0
    for (_ <- 0 until 64) {
      for (length <- lengths) {
        reverseSection(list, position, length)
        position = (position + length + skip) % 256
        skip += 1
      }
    }

    val denseHash = list.grouped(16).map(_.reduce(_ ^ _)).toArray
    denseHash.map(_.toByte).map("%02x".format(_)).mkString
  }

  def hexToBinary(hexStr: String): String = {
    hexStr.flatMap(c => f"${Integer.parseInt(c.toString, 16).toBinaryString}%4s".replace(' ', '0'))
  }

  def main(args: Array[String]): Unit = {
    val data = Source.fromFile("input.txt").getLines.mkString.trim

    var totalUsed = 0
    for (i <- 0 until 128) {
      val rowKey = s"$data-$i"
      val hash = knotHash(rowKey)
      val binaryRow = hexToBinary(hash)

      totalUsed += binaryRow.count(_ == '1')
    }

    println(totalUsed)
  }
}
