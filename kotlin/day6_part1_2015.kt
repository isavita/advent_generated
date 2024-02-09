import java.io.File

fun main(args: Array<String>) {
    val gridSize = 1000
    val grid = Array(gridSize) { BooleanArray(gridSize) }

    val instructions = File("input.txt").readLines()
    instructions.forEach { instruction ->
        processInstruction(instruction, grid)
    }

    println(countLights(grid))
}

fun processInstruction(instruction: String, grid: Array<BooleanArray>) {
    val parts = instruction.split(" ")
    val (startX, startY) = parts[parts.size - 3].split(",").map { it.toInt() }
    val (endX, endY) = parts[parts.size - 1].split(",").map { it.toInt() }

    for (x in startX..endX) {
        for (y in startY..endY) {
            when {
                instruction.startsWith("turn on") -> grid[x][y] = true
                instruction.startsWith("turn off") -> grid[x][y] = false
                instruction.startsWith("toggle") -> grid[x][y] = !grid[x][y]
            }
        }
    }
}

fun countLights(grid: Array<BooleanArray>): Int {
    var count = 0
    grid.forEach { row ->
        row.forEach { light ->
            if (light) {
                count++
            }
        }
    }
    return count
}