import java.io.File
import java.util.PriorityQueue

data class State(val x: Int, val y: Int, val dir: Char, val steps: Int, val heatLoss: Int)

fun main() {
    val input = File("input.txt").readLines()
    val grid = input.map { it.map { c -> c.digitToInt() } }
    val n = grid.size
    val m = grid[0].size

    val directions = listOf('>', 'v', '<', '^')
    val dx = mapOf('>' to 0, 'v' to 1, '<' to 0, '^' to -1)
    val dy = mapOf('>' to 1, 'v' to 0, '<' to -1, '^' to 0)

    val dist = Array(n) { Array(m) { Array(4) { Array(3) { Int.MAX_VALUE } } } }
    val pq = PriorityQueue<State>(compareBy { it.heatLoss })

    pq.add(State(0, 0, '>', 0, 0))
    pq.add(State(0, 0, 'v', 0, 0))

    while (pq.isNotEmpty()) {
        val (x, y, dir, steps, heatLoss) = pq.poll()

        if (x == n - 1 && y == m - 1) {
            println(heatLoss)
            return
        }

        if (dist[x][y][directions.indexOf(dir)][steps] < heatLoss) continue

        for (newDir in directions) {
            if (newDir == dir.opposite() || (newDir == dir && steps == 2)) continue
            val newSteps = if (newDir == dir) steps + 1 else 0
            val newX = x + dx[newDir]!!
            val newY = y + dy[newDir]!!

            if (newX in 0 until n && newY in 0 until m) {
                val newHeatLoss = heatLoss + grid[newX][newY]
                if (newHeatLoss < dist[newX][newY][directions.indexOf(newDir)][newSteps]) {
                    dist[newX][newY][directions.indexOf(newDir)][newSteps] = newHeatLoss
                    pq.add(State(newX, newY, newDir, newSteps, newHeatLoss))
                }
            }
        }
    }
}

fun Char.opposite(): Char = when (this) {
    '>' -> '<'
    'v' -> '^'
    '<' -> '>'
    '^' -> 'v'
    else -> throw IllegalArgumentException("Invalid direction")
}