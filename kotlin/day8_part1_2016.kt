import java.io.File

fun main() {
    val input = File("input.txt").readLines()

    val screen = Array(6) { BooleanArray(50) }

    input.forEach {
        val parts = it.split(" ")
        when {
            parts[0] == "rect" -> {
                val (a, b) = parts[1].split("x").map { it.toInt() }
                for (i in 0 until b) {
                    for (j in 0 until a) {
                        screen[i][j] = true
                    }
                }
            }
            parts[0] == "rotate" && parts[1] == "row" -> {
                val row = parts[2].split("=")[1].toInt()
                val shift = parts[4].toInt()
                val newRow = BooleanArray(50)
                for (i in 0 until 50) {
                    newRow[(i + shift) % 50] = screen[row][i]
                }
                screen[row] = newRow
            }
            parts[0] == "rotate" && parts[1] == "column" -> {
                val col = parts[2].split("=")[1].toInt()
                val shift = parts[4].toInt()
                val newCol = BooleanArray(6)
                for (i in 0 until 6) {
                    newCol[(i + shift) % 6] = screen[i][col]
                }
                for (i in 0 until 6) {
                    screen[i][col] = newCol[i]
                }
            }
        }
    }

    var litPixels = 0
    for (i in 0 until 6) {
        for (j in 0 until 50) {
            if (screen[i][j]) {
                litPixels++
            }
        }
    }

    println(litPixels)
}