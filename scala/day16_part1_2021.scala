
import scala.io.Source

object PacketDecoder {
  def hexToBinary(hex: String): String = {
    hex.map { c =>
      val binary = Integer.parseInt(c.toString, 16).toBinaryString
      binary.reverse.padTo(4, '0').reverse
    }.mkString
  }

  def parsePacket(binary: String): (Int, String, Int) = {
    if (binary.length < 6) return (0, "", 0)

    val version = Integer.parseInt(binary.take(3), 2)
    val typeId = Integer.parseInt(binary.slice(3, 6), 2)
    var remainingBits = binary.drop(6)
    var versionSum = version

    if (typeId == 4) {
      // Literal value packet
      var literalBinary = ""
      var continue = true
      while (continue) {
        val group = remainingBits.take(5)
        literalBinary += group.drop(1)
        continue = group.head == '1'
        remainingBits = remainingBits.drop(5)
      }
    } else {
      // Operator packet
      val lengthTypeId = remainingBits.head
      remainingBits = remainingBits.drop(1)

      if (lengthTypeId == '0') {
        // Total length of sub-packets
        val subPacketLength = Integer.parseInt(remainingBits.take(15), 2)
        remainingBits = remainingBits.drop(15)
        val subPacketBits = remainingBits.take(subPacketLength)
        var subRemainingBits = subPacketBits

        while (subRemainingBits.nonEmpty) {
          val (subVersionSum, subRemaining, _) = parsePacket(subRemainingBits)
          versionSum += subVersionSum
          subRemainingBits = subRemaining
        }

        remainingBits = remainingBits.drop(subPacketLength)
      } else {
        // Number of sub-packets
        val numSubPackets = Integer.parseInt(remainingBits.take(11), 2)
        remainingBits = remainingBits.drop(11)

        (0 until numSubPackets).foreach { _ =>
          val (subVersionSum, subRemaining, _) = parsePacket(remainingBits)
          versionSum += subVersionSum
          remainingBits = subRemaining
        }
      }
    }

    (versionSum, remainingBits, 0)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString.trim
    val binary = hexToBinary(input)
    val (versionSum, _, _) = parsePacket(binary)
    println(versionSum)
  }
}
