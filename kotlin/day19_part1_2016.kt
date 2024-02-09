import java.io.File

fun main(args: Array<String>) {
    val totalElves = readInput("input.txt")
    val winner = findWinningElf(totalElves)
    println(winner)
}

fun readInput(filename: String): Int {
    val totalElves = File(filename).readLines()[0].toInt()
    return totalElves
}

fun findWinningElf(totalElves: Int): Int {
    var highestPowerOfTwo = 1
    while (highestPowerOfTwo * 2 <= totalElves) {
        highestPowerOfTwo *= 2
    }
    return (totalElves - highestPowerOfTwo) * 2 + 1
}