import java.io.File

fun main() {
    val input = File("input.txt").readText().trim().toInt()

    fun isWall(x: Int, y: Int): Boolean {
        val num = x * x + 3 * x + 2 * x * y + y + y * y + input
        return num.countOneBits() % 2 != 0
    }

    fun bfs(startX: Int, startY: Int, maxSteps: Int): Int {
        val queue = ArrayDeque<Pair<Int, Int>>()
        val visited = mutableSetOf<Pair<Int, Int>>()
        queue.add(startX to startY)
        visited.add(startX to startY)
        var steps = 0

        while (queue.isNotEmpty() && steps < maxSteps) {
            val size = queue.size
            repeat(size) {
                val (x, y) = queue.removeFirst()
                for ((dx, dy) in listOf(0 to 1, 0 to -1, 1 to 0, -1 to 0)) {
                    val newX = x + dx
                    val newY = y + dy
                    if (newX >= 0 && newY >= 0 && !isWall(newX, newY) && (newX to newY !in visited)) {
                        queue.add(newX to newY)
                        visited.add(newX to newY)
                    }
                }
            }
            steps++
        }

        return visited.size
    }

    println(bfs(1, 1, 50))
}