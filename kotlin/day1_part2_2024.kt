import java.io.File

fun main() {
    val lines = File("input.txt").readLines().filter { it.isNotBlank() }
    val leftList = mutableListOf<Int>()
    val rightNumbers = mutableMapOf<Int, Int>()

    for (line in lines) {
        val numbers = line.trim().split("\\s+".toRegex()).map { it.toInt() }
        leftList.add(numbers.first())
        rightNumbers[numbers.last()] = rightNumbers.getOrDefault(numbers.last(), 0) + 1
    }

    val similarityScore = leftList.sumOf { num ->
        num.toLong() * rightNumbers.getOrDefault(num, 0)
    }

    println("Similarity score: $similarityScore")
}
