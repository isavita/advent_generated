import java.io.File

fun main(args: Array<String>) {
    val heightmap = mutableListOf<MutableList<Int>>()

    File("input.txt").forEachLine {
        val row = it.map { char -> char.toString().toInt() }.toMutableList()
        heightmap.add(row)
    }

    var totalRiskLevel = 0
    for (y in heightmap.indices) {
        for (x in heightmap[y].indices) {
            if (isLowPoint(heightmap, x, y)) {
                totalRiskLevel += 1 + heightmap[y][x]
            }
        }
    }

    println(totalRiskLevel)
}

fun isLowPoint(heightmap: List<List<Int>>, x: Int, y: Int): Boolean {
    val height = heightmap[y][x]
    if (x > 0 && heightmap[y][x-1] <= height) {
        return false
    }
    if (x < heightmap[y].size - 1 && heightmap[y][x+1] <= height) {
        return false
    }
    if (y > 0 && heightmap[y-1][x] <= height) {
        return false
    }
    if (y < heightmap.size - 1 && heightmap[y+1][x] <= height) {
        return false
    }
    return true
}