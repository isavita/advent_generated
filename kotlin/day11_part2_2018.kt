import java.io.File

fun main() {
    val data = File("input.txt").readText().trim()
    val serial = data.toInt()

    val gridSize = 300
    val grid = Array(gridSize) { Array(gridSize) { 0 } }

    for (y in 0 until gridSize) {
        for (x in 0 until gridSize) {
            var rackID = x + 11
            var powerLevel = rackID * (y + 1)
            powerLevel += serial
            powerLevel *= rackID
            powerLevel = (powerLevel / 100) % 10
            powerLevel -= 5
            grid[y][x] = powerLevel
        }
    }

    var maxPower = Int.MIN_VALUE
    var maxX = 0
    var maxY = 0
    var maxSize = 0

    for (size in 1..gridSize) {
        for (y in 0..gridSize - size) {
            for (x in 0..gridSize - size) {
                var totalPower = 0
                for (dy in 0 until size) {
                    for (dx in 0 until size) {
                        totalPower += grid[y + dy][x + dx]
                    }
                }
                if (totalPower > maxPower) {
                    maxPower = totalPower
                    maxX = x + 1
                    maxY = y + 1
                    maxSize = size
                }
            }
        }
    }

    println("$maxX,$maxY,$maxSize")
}