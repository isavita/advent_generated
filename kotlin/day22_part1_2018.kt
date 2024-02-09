import java.io.File

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()
    val depth = lines[0].split(" ")[1].toInt()
    val coords = lines[1].split(" ")[1]
    val parts = coords.split(",")
    val x = parts[0].toInt()
    val y = parts[1].toInt()

    val cave = makeCaveSystem(depth, intArrayOf(x, y))
    val riskLevel = calculateRiskLevel(cave, intArrayOf(x, y))
    println("Total Risk Level: $riskLevel")
}

fun makeCaveSystem(depth: Int, target: IntArray): Array<IntArray> {
    val cave = Array(target[1] + 1) { IntArray(target[0] + 1) }
    for (y in cave.indices) {
        for (x in cave[y].indices) {
            val geologicIndex = when {
                (x == 0 && y == 0) || (x == target[0] && y == target[1]) -> 0
                y == 0 -> x * 16807
                x == 0 -> y * 48271
                else -> cave[y][x - 1] * cave[y - 1][x]
            }
            cave[y][x] = (geologicIndex + depth) % 20183
        }
    }
    return cave
}

fun calculateRiskLevel(cave: Array<IntArray>, target: IntArray): Int {
    var riskLevel = 0
    for (y in 0..target[1]) {
        for (x in 0..target[0]) {
            riskLevel += cave[y][x] % 3
        }
    }
    return riskLevel
}