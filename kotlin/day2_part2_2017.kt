import java.io.File

fun main() {
    val inputFile = File("input.txt")
    val lines = inputFile.readLines()

    val partOneChecksum = calculatePartOneChecksum(lines)
    val partTwoChecksum = calculatePartTwoChecksum(lines)

    println("Part One Checksum: $partOneChecksum")
    println("Part Two Checksum: $partTwoChecksum")
}

fun calculatePartOneChecksum(lines: List<String>): Int {
    return lines.sumOf { line ->
        val numbers = line.split("\\s+".toRegex()).map { it.toInt() }
        numbers.maxOrNull()!! - numbers.minOrNull()!!
    }
}

fun calculatePartTwoChecksum(lines: List<String>): Int {
    return lines.sumOf { line ->
        val numbers = line.split("\\s+".toRegex()).map { it.toInt() }
        for (i in 0 until numbers.size) {
            for (j in i + 1 until numbers.size) {
                val a = numbers[i]
                val b = numbers[j]
                if (a % b == 0) return@sumOf a / b
                if (b % a == 0) return@sumOf b / a
            }
        }
        0
    }
}