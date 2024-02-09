import java.io.File

fun main(args: Array<String>) {
    val grid = mutableMapOf<Pair<Int, Int>, Int>()
    val lines = File("input.txt").readLines()

    var y = 0
    for (line in lines) {
        line.forEachIndexed { x, c ->
            grid[x to y] = c.toString().toInt()
        }
        y++
    }

    val neighbors4 = listOf(0 to 1, 0 to -1, 1 to 0, -1 to 0)
    var maxScore = 0

    for (p in grid.keys) {
        var score = 1
        for (n in neighbors4) {
            var (next, view) = p to 0
            while (true) {
                next = next.first + n.first to next.second + n.second
                if (grid.containsKey(next)) {
                    view++
                    if (grid[next]!! >= grid[p]!!) {
                        score *= view
                        break
                    }
                } else {
                    score *= view
                    break
                }
            }
        }

        if (score > maxScore) {
            maxScore = score
        }
    }
    println(maxScore)
}