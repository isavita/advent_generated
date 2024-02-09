import java.io.File

fun main(args: Array<String>) {
    val heightmap = mutableListOf<List<Int>>()
    File("input.txt").forEachLine {
        heightmap.add(it.map { char -> Character.getNumericValue(char) })
    }

    val basinSizes = mutableListOf<Int>()
    val visited = mutableMapOf<Pair<Int, Int>, Boolean>()

    heightmap.forEachIndexed { y, row ->
        row.forEachIndexed { x, _ ->
            if (isLowPoint(heightmap, x, y)) {
                val size = exploreBasin(heightmap, x, y, visited)
                basinSizes.add(size)
            }
        }
    }

    basinSizes.sortDescending()
    val result = basinSizes[0] * basinSizes[1] * basinSizes[2]
    println(result)
}

fun isLowPoint(heightmap: List<List<Int>>, x: Int, y: Int): Boolean {
    val height = heightmap[y][x]
    if (x > 0 && heightmap[y][x - 1] <= height) {
        return false
    }
    if (x < heightmap[y].size - 1 && heightmap[y][x + 1] <= height) {
        return false
    }
    if (y > 0 && heightmap[y - 1][x] <= height) {
        return false
    }
    if (y < heightmap.size - 1 && heightmap[y + 1][x] <= height) {
        return false
    }
    return true
}

fun exploreBasin(heightmap: List<List<Int>>, x: Int, y: Int, visited: MutableMap<Pair<Int, Int>, Boolean>): Int {
    if (visited[Pair(x, y)] == true || heightmap[y][x] == 9) {
        return 0
    }
    visited[Pair(x, y)] = true
    var size = 1

    val directions = listOf(listOf(0, -1), listOf(-1, 0), listOf(0, 1), listOf(1, 0))
    directions.forEach { dir ->
        val newX = x + dir[0]
        val newY = y + dir[1]
        if (newX >= 0 && newX < heightmap[0].size && newY >= 0 && newY < heightmap.size) {
            size += exploreBasin(heightmap, newX, newY, visited)
        }
    }
    return size
}