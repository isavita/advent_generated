import java.io.File

fun main() {
    val checksum = File("input.txt")
        .readLines()
        .map { line ->
            val numbers = line.split("\\s+".toRegex()).map { it.toInt() }
            numbers.maxOrNull()!! - numbers.minOrNull()!!
        }
        .sum()

    println("Checksum: $checksum")
}