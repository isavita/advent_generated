import java.io.File

fun main() {
    val input = File("input.txt").readText().trim()
    val binaryString = hexToBinary(input)
    val (versionSum, value) = parsePacket(binaryString)

    println("Sum of version numbers: $versionSum")
    println("Value of the outermost packet: $value")
}

fun hexToBinary(hex: String): String {
    return hex.map { it.toString().toInt(16).toString(2).padStart(4, '0') }.joinToString("")
}

fun parsePacket(binary: String): Pair<Int, Long> {
    var versionSum = 0
    var index = 0

    fun readBits(n: Int): String {
        val bits = binary.substring(index, index + n)
        index += n
        return bits
    }

    fun parseLiteral(): Long {
        var literalValue = ""
        while (true) {
            val group = readBits(5)
            literalValue += group.substring(1)
            if (group[0] == '0') break
        }
        return literalValue.toLong(2)
    }

    fun parsePacketRec(): Pair<Int, Long> {
        val version = readBits(3).toInt(2)
        versionSum += version
        val typeID = readBits(3).toInt(2)

        return when (typeID) {
            4 -> version to parseLiteral()
            else -> {
                val lengthTypeID = readBits(1)
                val subPackets = mutableListOf<Pair<Int, Long>>()

                if (lengthTypeID == "0") {
                    val length = readBits(15).toInt(2)
                    val endIndex = index + length
                    while (index < endIndex) {
                        subPackets.add(parsePacketRec())
                    }
                } else {
                    val numSubPackets = readBits(11).toInt(2)
                    repeat(numSubPackets) {
                        subPackets.add(parsePacketRec())
                    }
                }

                val subVersions = subPackets.sumOf { it.first }
                val subValues = subPackets.map { it.second }
                val result = when (typeID) {
                    0 -> subValues.sum()
                    1 -> subValues.reduce { acc, v -> acc * v }
                    2 -> subValues.minOrNull() ?: 0
                    3 -> subValues.maxOrNull() ?: 0
                    5 -> if (subValues[0] > subValues[1]) 1 else 0
                    6 -> if (subValues[0] < subValues[1]) 1 else 0
                    7 -> if (subValues[0] == subValues[1]) 1 else 0
                    else -> 0
                }
                version + subVersions to result
            }
        }
    }

    return parsePacketRec()
}