import java.io.File

fun main(args: Array<String>) {
    val initialSequence = readInput("input.txt")
    val result = lookAndSay(initialSequence, 50)
    println(result.length)
}

fun readInput(filename: String): String {
    return File(filename).readLines().first()
}

fun lookAndSay(sequence: String, iterations: Int): String {
    var currentSequence = sequence
    repeat(iterations) {
        currentSequence = nextSequence(currentSequence)
    }
    return currentSequence
}

fun nextSequence(sequence: String): String {
    val result = StringBuilder()
    var i = 0
    while (i < sequence.length) {
        var count = 1
        val digit = sequence[i]
        var j = i + 1
        while (j < sequence.length && sequence[j] == digit) {
            count++
            j++
        }
        result.append("$count$digit")
        i += count
    }
    return result.toString()
}