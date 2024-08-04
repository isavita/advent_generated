import java.io.File

// Function to compare two packets
fun comparePackets(left: Any, right: Any): Int {
    return when {
        left is Int && right is Int -> left.compareTo(right)
        left is List<*> && right is List<*> -> {
            for (i in 0 until minOf(left.size, right.size)) {
                val result = comparePackets(left[i]!!, right[i]!!)
                if (result != 0) return result
            }
            left.size.compareTo(right.size)
        }
        left is Int -> comparePackets(listOf(left), right)
        right is Int -> comparePackets(left, listOf(right))
        else -> throw IllegalArgumentException("Invalid packet type")
    }
}

// Function to parse a packet string into a list
fun parsePacket(packet: String): List<Any> {
    val stack = mutableListOf<MutableList<Any>>()
    var currentList = mutableListOf<Any>()
    var i = 0
    while (i < packet.length) {
        when (val char = packet[i]) {
            '[' -> {
                stack.add(currentList)
                currentList = mutableListOf()
            }
            ']' -> {
                val lastList = stack.removeLast()
                lastList.add(currentList)
                currentList = lastList
            }
            ',' -> {
                // Do nothing, just move to the next character
            }
            else -> {
                var numStr = ""
                while (i < packet.length && packet[i].isDigit()) {
                    numStr += packet[i]
                    i++
                }
                currentList.add(numStr.toInt())
                continue
            }
        }
        i++
    }
    return currentList
}

fun main() {
    val inputFile = File("input.txt")
    val lines = inputFile.readLines()

    val packets = mutableListOf<Pair<List<Any>, List<Any>>>()
    var i = 0
    while (i < lines.size) {
        val left = parsePacket(lines[i])
        val right = parsePacket(lines[i + 1])
        packets.add(Pair(left, right))
        i += 3
    }

    var sumIndices = 0
    for ((index, pair) in packets.withIndex()) {
        if (comparePackets(pair.first, pair.second) < 0) {
            sumIndices += index + 1
        }
    }

    println("Sum of indices of pairs in the right order: $sumIndices")
}