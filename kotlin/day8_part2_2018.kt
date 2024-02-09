import java.io.File

fun main(args: Array<String>) {
    val numbers = readInput("input.txt")
    val value = parseTree(numbers, 0).first
    println(value)
}

fun readInput(filename: String): List<Int> {
    val line = File(filename).readLines()[0]
    return line.split(" ").map { it.toInt() }
}

fun parseTree(data: List<Int>, index: Int): Pair<Int, Int> {
    var currentIndex = index
    val childCount = data[currentIndex]
    val metaCount = data[currentIndex + 1]
    currentIndex += 2

    val childValues = mutableListOf<Int>()
    repeat(childCount) {
        val (childValue, newIndex) = parseTree(data, currentIndex)
        childValues.add(childValue)
        currentIndex = newIndex
    }

    var value = 0
    if (childCount == 0) {
        for (i in 0 until metaCount) {
            value += data[currentIndex + i]
        }
    } else {
        for (i in 0 until metaCount) {
            val metadata = data[currentIndex + i]
            if (metadata <= childCount && metadata > 0) {
                value += childValues[metadata - 1]
            }
        }
    }
    currentIndex += metaCount

    return Pair(value, currentIndex)
}