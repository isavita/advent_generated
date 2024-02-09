import java.io.File

fun main() {
    val grid = Array(1000) { IntArray(1000) }

    File("input.txt").forEachLine { line ->
        val instruction = line.split(" ")
        val start = instruction[instruction.size - 3].split(",").map { it.toInt() }
        val end = instruction[instruction.size - 1].split(",").map { it.toInt() }

        when {
            instruction[1] == "on" -> {
                for (i in start[0]..end[0]) {
                    for (j in start[1]..end[1]) {
                        grid[i][j]++
                    }
                }
            }
            instruction[1] == "off" -> {
                for (i in start[0]..end[0]) {
                    for (j in start[1]..end[1]) {
                        if (grid[i][j] > 0) {
                            grid[i][j]--
                        }
                    }
                }
            }
            instruction[0] == "toggle" -> {
                for (i in start[0]..end[0]) {
                    for (j in start[1]..end[1]) {
                        grid[i][j] += 2
                    }
                }
            }
        }
    }

    var totalBrightness = 0
    for (i in grid.indices) {
        for (j in grid[i]) {
            totalBrightness += j
        }
    }

    println(totalBrightness)
}