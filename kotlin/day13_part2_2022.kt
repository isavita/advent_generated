
import java.io.File

sealed class Packet {
    data class Integer(val value: Int) : Packet()
    data class List(val values: kotlin.collections.List<Packet>) : Packet()
}

fun parsePacket(input: String): Packet {
    if (input.startsWith("[")) {
        if (input == "[]") return Packet.List(emptyList())
        var remaining = input.substring(1, input.length - 1)
        val values = mutableListOf<Packet>()
        var level = 0
        var start = 0
        for (i in remaining.indices) {
            when (remaining[i]) {
                '[' -> level++
                ']' -> level--
                ',' -> if (level == 0) {
                    values.add(parsePacket(remaining.substring(start, i)))
                    start = i + 1
                }
            }
        }
        values.add(parsePacket(remaining.substring(start)))
        return Packet.List(values)
    } else {
        return Packet.Integer(input.toInt())
    }
}

fun comparePackets(left: Packet, right: Packet): Int {
    return when {
        left is Packet.Integer && right is Packet.Integer -> left.value.compareTo(right.value)
        left is Packet.List && right is Packet.List -> {
            for (i in 0 until kotlin.math.max(left.values.size, right.values.size)) {
                val leftValue = left.values.getOrNull(i)
                val rightValue = right.values.getOrNull(i)
                when {
                    leftValue == null -> return -1
                    rightValue == null -> return 1
                    else -> {
                        val comparison = comparePackets(leftValue, rightValue)
                        if (comparison != 0) return comparison
                    }
                }
            }
            0
        }
        left is Packet.Integer -> comparePackets(Packet.List(listOf(left)), right)
        right is Packet.Integer -> comparePackets(left, Packet.List(listOf(right)))
        else -> throw IllegalArgumentException("Invalid packet types")
    }
}

fun main() {
    val lines = File("input.txt").readLines()
    var sumOfIndices = 0
    val allPackets = mutableListOf<Packet>()
    var pairIndex = 1
    for (i in lines.indices step 3) {
        val left = parsePacket(lines[i])
        val right = parsePacket(lines[i + 1])
        allPackets.add(left)
        allPackets.add(right)
        if (comparePackets(left, right) < 0) {
            sumOfIndices += pairIndex
        }
        pairIndex++
    }

    println("Part 1: $sumOfIndices")

    val divider1 = parsePacket("[[2]]")
    val divider2 = parsePacket("[[6]]")
    allPackets.add(divider1)
    allPackets.add(divider2)

    allPackets.sortWith(::comparePackets)

    val index1 = allPackets.indexOf(divider1) + 1
    val index2 = allPackets.indexOf(divider2) + 1
    println("Part 2: ${index1 * index2}")
}
