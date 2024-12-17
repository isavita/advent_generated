
import scala.io.Source

object Main {
  def hexToBin(hex: String): String =
    hex.flatMap(c => String.format("%4s", Integer.parseInt(c.toString, 16).toBinaryString).replace(' ', '0'))

  def parsePacket(binStr: String, idx: Int): (Int, Int, Long) = {
    val version = Integer.parseInt(binStr.substring(idx, idx + 3), 2)
    val typeID = Integer.parseInt(binStr.substring(idx + 3, idx + 6), 2)
    var currentIdx = idx + 6

    if (typeID == 4) {
      var value = 0L
      var continue = true
      while (continue) {
        value = (value << 4) | Integer.parseInt(binStr.substring(currentIdx + 1, currentIdx + 5), 2)
        if (binStr(currentIdx) == '0') {
          continue = false
        }
        currentIdx += 5
      }
      (version, currentIdx, value)
    } else {
      val lengthTypeID = binStr(currentIdx) - '0'
      currentIdx += 1
      var numSubPackets = 0
      var subPacketLength = 0

      if (lengthTypeID == 0) {
        subPacketLength = Integer.parseInt(binStr.substring(currentIdx, currentIdx + 15), 2)
        currentIdx += 15
      } else {
        numSubPackets = Integer.parseInt(binStr.substring(currentIdx, currentIdx + 11), 2)
        currentIdx += 11
      }

      var values = List[Long]()
      while (
        (lengthTypeID == 0 && subPacketLength > 0) ||
        (lengthTypeID == 1 && numSubPackets > 0)
      ) {
        val (subVersion, newIndex, subValue) = parsePacket(binStr, currentIdx)
        values = values :+ subValue
        if (lengthTypeID == 0) {
          subPacketLength -= (newIndex - currentIdx)
        } else {
          numSubPackets -= 1
        }
        currentIdx = newIndex
      }

      val result = typeID match {
        case 0 => values.sum
        case 1 => values.product
        case 2 => values.min
        case 3 => values.max
        case 5 => if (values(0) > values(1)) 1L else 0L
        case 6 => if (values(0) < values(1)) 1L else 0L
        case 7 => if (values(0) == values(1)) 1L else 0L
        case _ => throw new IllegalArgumentException("Unknown typeID")
      }
      (version, currentIdx, result)
    }
  }

  def main(args: Array[String]): Unit = {
    val hexStr = Source.fromFile("input.txt").getLines().mkString.trim
    val binStr = hexToBin(hexStr)
    val (_, _, value) = parsePacket(binStr, 0)
    println(value)
  }
}
