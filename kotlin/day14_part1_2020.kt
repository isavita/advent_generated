import java.io.File
import java.util.regex.Pattern

fun applyMask(value: Long, mask: String): Long {
    var result: Long = 0
    for (i in 0 until 36) {
        val bitValue: Long = 1L shl (35 - i)
        when {
            mask[i] == '1' -> result = result or bitValue
            mask[i] == 'X' -> result = result or (value and bitValue)
        }
    }
    return result
}

fun main(args: Array<String>) {
    val file = File("input.txt")
    val maskRegex = Regex("mask = (.*)")
    val memRegex = Regex("mem\\[(\\d+)] = (\\d+)")
    var mask = ""
    val mem = mutableMapOf<Long, Long>()

    file.forEachLine {
        when {
            it.startsWith("mask = ") -> mask = maskRegex.find(it)!!.groupValues[1]
            else -> {
                val matchResult = memRegex.find(it)!!
                val address = matchResult.groupValues[1].toLong()
                val value = matchResult.groupValues[2].toLong()
                mem[address] = applyMask(value, mask)
            }
        }
    }

    val sum = mem.values.sum()
    println(sum)
}