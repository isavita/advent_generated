
import java.io.File
import java.math.BigInteger

fun trimLeadingZeros(s: String): String {
    var i = 0
    while (i < s.length - 1 && s[i] == '0') {
        i++
    }
    return s.substring(i)
}

fun splitStone(s: String): Pair<String, String> {
    val mid = s.length / 2
    var left = trimLeadingZeros(s.substring(0, mid))
    var right = trimLeadingZeros(s.substring(mid))
    if (left.isEmpty()) {
        left = "0"
    }
    if (right.isEmpty()) {
        right = "0"
    }
    return Pair(left, right)
}

fun multiplyBy2024(s: String): String {
    return (BigInteger(s) * BigInteger("2024")).toString()
}

fun main() {
    val stonesStr = File("input.txt").readLines().firstOrNull()?.split(" ") ?: return
    var stonesMap = mutableMapOf<String, Long>()
    for (s in stonesStr) {
        stonesMap[s] = stonesMap.getOrDefault(s, 0) + 1
    }

    repeat(75) {
        val newStonesMap = mutableMapOf<String, Long>()
        for ((stone, count) in stonesMap) {
            when {
                stone == "0" -> newStonesMap["1"] = newStonesMap.getOrDefault("1", 0) + count
                stone.length % 2 == 0 -> {
                    val (left, right) = splitStone(stone)
                    newStonesMap[left] = newStonesMap.getOrDefault(left, 0) + count
                    newStonesMap[right] = newStonesMap.getOrDefault(right, 0) + count
                }
                else -> {
                    val newStone = multiplyBy2024(stone)
                    newStonesMap[newStone] = newStonesMap.getOrDefault(newStone, 0) + count
                }
            }
        }
        stonesMap = newStonesMap
    }

    val totalStones = stonesMap.values.sum()
    println(totalStones)
}
