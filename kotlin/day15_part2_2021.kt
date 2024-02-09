import java.io.File
import java.util.*

data class Position(val x: Int, val y: Int, val risk: Int)

class Solution {
    fun dijkstra(grid: Array<IntArray>): Int {
        val pq = PriorityQueue { a: Position, b: Position -> a.risk.compareTo(b.risk) }
        pq.add(Position(0, 0, 0))

        val rows = grid.size
        val cols = grid[0].size
        val dist = Array(rows) { IntArray(cols) { Int.MAX_VALUE } }
        dist[0][0] = 0

        val directions = listOf(Position(1, 0, 0), Position(0, 1, 0), Position(-1, 0, 0), Position(0, -1, 0))

        while (pq.isNotEmpty()) {
            val curr = pq.poll()
            if (curr.x == rows - 1 && curr.y == cols - 1) {
                return curr.risk
            }
            for (d in directions) {
                val nx = curr.x + d.x
                val ny = curr.y + d.y
                if (nx >= 0 && ny >= 0 && nx < rows && ny < cols) {
                    val nextRisk = curr.risk + grid[nx][ny]
                    if (nextRisk < dist[nx][ny]) {
                        dist[nx][ny] = nextRisk
                        pq.add(Position(nx, ny, nextRisk))
                    }
                }
            }
        }
        return -1
    }

    fun extendGrid(initialGrid: Array<IntArray>): Array<IntArray> {
        val rows = initialGrid.size
        val cols = initialGrid[0].size
        val extendedGrid = Array(rows * 5) { IntArray(cols * 5) }
        for (i in 0 until rows * 5) {
            for (j in 0 until cols * 5) {
                val newRisk = initialGrid[i % rows][j % cols] + i / rows + j / cols
                val risk = if (newRisk > 9) newRisk - 9 else newRisk
                extendedGrid[i][j] = risk
            }
        }
        return extendedGrid
    }
}

fun main(args: Array<String>) {
    val solution = Solution()
    val file = File("input.txt")
    val scanner = Scanner(file)

    val initialGrid = mutableListOf<IntArray>()
    while (scanner.hasNextLine()) {
        val line = scanner.nextLine()
        val row = line.map { it.toString().toInt() }.toIntArray()
        initialGrid.add(row)
    }

    val extendedGrid = solution.extendGrid(initialGrid.toTypedArray())
    println(solution.dijkstra(extendedGrid))
}