import java.io.File

fun hexToBin(hex: String): String {
    var bin = ""
    for (h in hex) {
        val b = h.toString().toInt(16)
        bin += String.format("%04d", Integer.toBinaryString(b.toInt()).toInt())
    }
    return bin
}

fun parsePacket(binStr: String, idx: Int): Pair<Int, Int> {
    var index = idx
    var version = (binStr[index] - '0') shl 2 or ((binStr[index + 1] - '0') shl 1) or (binStr[index + 2] - '0')
    var typeID = (binStr[index + 3] - '0') shl 2 or ((binStr[index + 4] - '0') shl 1) or (binStr[index + 5] - '0')
    index += 6

    if (typeID == 4) {
        while (binStr[index] == '1') {
            index += 5
        }
        index += 5
        return Pair(version, index)
    }

    var lengthTypeID = binStr[index] - '0'
    index++
    var numSubPackets = 0
    var subPacketLength = 0

    if (lengthTypeID == 0) {
        subPacketLength = 0
        for (i in 0 until 15) {
            subPacketLength = subPacketLength shl 1 or (binStr[index] - '0')
            index++
        }
    } else {
        numSubPackets = 0
        for (i in 0 until 11) {
            numSubPackets = numSubPackets shl 1 or (binStr[index] - '0')
            index++
        }
    }

    var versionSum = version
    while (true) {
        if ((lengthTypeID == 0 && subPacketLength == 0) || (lengthTypeID == 1 && numSubPackets == 0)) {
            break
        }
        val (subVersion, newIndex) = parsePacket(binStr, index)
        versionSum += subVersion

        if (lengthTypeID == 0) {
            subPacketLength -= newIndex - index
        } else {
            numSubPackets--
        }
        index = newIndex
    }
    return Pair(versionSum, index)
}

fun main(args: Array<String>) {
    val hexStr = File("input.txt").readText().trim()
    val binStr = hexToBin(hexStr)
    val (versionSum, _) = parsePacket(binStr, 0)
    println(versionSum)
}