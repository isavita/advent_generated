import java.io.File
import java.util.*

fun main() {
    val n = cleaningRobot(File("input.txt").readText().trim())
    println(n)
}

fun cleaningRobot(input: String): Int {
    val grid = input.lines().map { it.toCharArray().map { char -> char.toString() } }
    val graph = mutableListOf<IntArray>()

    for (r in grid.indices) {
        for (c in grid[r].indices) {
            val cell = grid[r][c]
            if (cell.matches(Regex("[0-9]"))) {
                val poi = cell
                val distancesFromPOI = bfsGetEdgeWeights(grid, r to c)

                if (graph.isEmpty()) {
                    repeat(distancesFromPOI.size) { graph.add(IntArray(distancesFromPOI.size)) }
                }
                graph[poi.toInt()] = distancesFromPOI
            }
        }
    }

    return dfs(graph, 0, mutableSetOf(0), true)
}

data class BfsNode(val row: Int, val col: Int, val distance: Int)

fun bfsGetEdgeWeights(grid: List<List<String>>, start: Pair<Int, Int>): IntArray {
    val poiToDistance = mutableMapOf(grid[start.first][start.second] to 0)
    val queue: Queue<BfsNode> = LinkedList()
    queue.add(BfsNode(start.first, start.second, 0))
    val visited = mutableSetOf<Pair<Int, Int>>()

    while (queue.isNotEmpty()) {
        val front = queue.poll()
        if (!visited.add(front.row to front.col)) continue

        if (grid[front.row][front.col].matches(Regex("[0-9]"))) {
            poiToDistance[grid[front.row][front.col]] = front.distance
        }

        for (d in dirs) {
            val nextRow = front.row + d.first
            val nextCol = front.col + d.second
            if (nextRow in grid.indices && nextCol in grid[nextRow].indices && grid[nextRow][nextCol] != "#") {
                queue.add(BfsNode(nextRow, nextCol, front.distance + 1))
            }
        }
    }

    return IntArray(poiToDistance.size) { poiToDistance[it.toString()] ?: Int.MAX_VALUE }
}

val dirs = arrayOf(0 to -1, 0 to 1, 1 to 0, -1 to 0)

fun dfs(graph: List<IntArray>, entryIndex: Int, visited: MutableSet<Int>, returnToZero: Boolean): Int {
    if (visited.size == graph.size) {
        return if (returnToZero) graph[entryIndex][0] else 0
    }

    var minDistance = Int.MAX_VALUE
    for (i in graph[entryIndex].indices) {
        if (i !in visited) {
            visited.add(i)
            val dist = graph[entryIndex][i] + dfs(graph, i, visited, returnToZero)
            minDistance = minOf(minDistance, dist)
            visited.remove(i)
        }
    }
    return minDistance
}