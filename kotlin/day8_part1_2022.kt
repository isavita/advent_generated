import java.io.File

fun main(args: Array<String>) {
    val grid = mutableMapOf<Pair<Int, Int>, Int>()
    val visible = mutableSetOf<Pair<Int, Int>>()
    val lines = File("input.txt").readLines()
    for (y in lines.indices) {
        val line = lines[y]
        for (x in line.indices) {
            grid[x to y] = line[x].toString().toInt()
        }
    }

    val neighbors4 = listOf(0 to 1, 0 to -1, 1 to 0, -1 to 0)
    for (p in grid.keys) {
        for (n in neighbors4) {
            var next = p
            while (true) {
                next = next.first + n.first to next.second + n.second
                if (grid.containsKey(next)) {
                    if (grid[next]!! >= grid[p]!!) {
                        break
                    }
                } else {
                    visible.add(p)
                    break
                }
            }
        }
    }
    println(visible.size)
}