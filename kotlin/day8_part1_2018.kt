import java.io.File

fun main(args: Array<String>) {
    val numbers = readInput("input.txt")
    val result = parseTree(numbers, 0)
    println(result.first)
}

fun readInput(filename: String): List<Int> {
    val line = File(filename).readText()
    return line.split(" ").map { it.toInt() }
}

fun parseTree(data: List<Int>, index: Int): Pair<Int, Int> {
    var currentIndex = index
    val childCount = data[currentIndex]
    val metaCount = data[currentIndex + 1]
    currentIndex += 2

    var sum = 0
    repeat(childCount) {
        val (childSum, newIndex) = parseTree(data, currentIndex)
        sum += childSum
        currentIndex = newIndex
    }

    repeat(metaCount) {
        sum += data[currentIndex + it]
    }
    currentIndex += metaCount

    return Pair(sum, currentIndex)
}