import java.io.File

data class Point(val x: Int, val y: Int)

data class State(val pos: Point, val keys: Int)

fun main() {
    val lines = File("input.txt").readLines()
    val grid = lines.toTypedArray()
    var start = Point(0, 0)
    val keyMap = mutableMapOf<Char, Int>()
    var keyCounter = 0

    for (y in lines.indices) {
        val line = lines[y]
        for (x in line.indices) {
            when (line[x]) {
                '@' -> start = Point(x, y)
                in 'a'..'z' -> {
                    keyMap[line[x]] = keyCounter
                    keyCounter++
                }
            }
        }
    }

    println(findShortestPath(grid, start, keyMap))
}

fun findShortestPath(grid: Array<String>, start: Point, keyMap: Map<Char, Int>): Int {
    val dirs = arrayOf(Point(0, -1), Point(-1, 0), Point(0, 1), Point(1, 0))
    val visited = mutableMapOf<State, Boolean>()
    val queue = mutableListOf(State(start, 0))
    var steps = 0

    while (queue.isNotEmpty()) {
        val size = queue.size
        for (i in 0 until size) {
            val current = queue.removeAt(0)

            if (current.keys == (1 shl keyMap.size) - 1) {
                return steps
            }

            for (d in dirs) {
                val next = Point(current.pos.x + d.x, current.pos.y + d.y)
                if (next.x in 0 until grid[0].length && next.y in 0 until grid.size) {
                    val char = grid[next.y][next.x]
                    if (char != '#' && !(char in 'A'..'Z' && current.keys and (1 shl keyMap.getOrDefault(char.toLowerCase(), 0)) == 0)) {
                        var newState = State(next, current.keys)
                        if (char in 'a'..'z') {
                            newState = newState.copy(keys = newState.keys or (1 shl keyMap[char]!!))
                        }
                        if (!visited.containsKey(newState)) {
                            visited[newState] = true
                            queue.add(newState)
                        }
                    }
                }
            }
        }
        steps++
    }
    return -1
}