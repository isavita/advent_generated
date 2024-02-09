import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim().toInt()

    val gridSize = 300
    val grid = Array(gridSize) { IntArray(gridSize) }

    for (y in 0 until gridSize) {
        for (x in 0 until gridSize) {
            val rackID = x + 11
            var powerLevel = rackID * (y + 1)
            powerLevel += input
            powerLevel *= rackID
            powerLevel = (powerLevel / 100) % 10
            powerLevel -= 5
            grid[y][x] = powerLevel
        }
    }

    var maxPower = Int.MIN_VALUE
    var maxX = 0
    var maxY = 0
    for (y in 0 until gridSize - 2) {
        for (x in 0 until gridSize - 2) {
            var totalPower = 0
            for (dy in 0 until 3) {
                for (dx in 0 until 3) {
                    totalPower += grid[y + dy][x + dx]
                }
            }
            if (totalPower > maxPower) {
                maxPower = totalPower
                maxX = x + 1
                maxY = y + 1
            }
        }
    }

    println("$maxX,$maxY")
}