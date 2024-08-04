import java.io.File

fun main() {
    val input = File("input.txt").readLines()
    val x = mutableListOf(1)
    val signalStrengths = mutableListOf<Int>()
    val crt = Array(6) { CharArray(40) { '.' } }

    for (line in input) {
        val parts = line.split(" ")
        if (parts[0] == "noop") {
            x.add(x.last())
        } else if (parts[0] == "addx") {
            x.add(x.last())
            x.add(x.last() + parts[1].toInt())
        }
    }

    for (i in 20..x.size step 40) {
        signalStrengths.add(i * x[i - 1])
    }

    for (i in x.indices) {
        val row = i / 40
        val col = i % 40
        if (x[i] - 1 <= col && col <= x[i] + 1) {
            crt[row][col] = '#'
        }
    }

    println("Part 1: ${signalStrengths.sum()}")
    println("Part 2:")
    for (row in crt) {
        println(row.joinToString(""))
    }
}