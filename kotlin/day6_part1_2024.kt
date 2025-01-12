
import java.io.File

fun main() {
    val grid = File("input.txt").readLines().map { it.toCharArray() }
    val h = grid.size
    val w = grid[0].size
    val dirs = arrayOf(0 to -1, 1 to 0, 0 to 1, -1 to 0)
    var x = 0
    var y = 0
    var dirIdx = 0
    var found = false

    for (i in 0 until h) {
        for (j in 0 until w) {
            when (grid[i][j]) {
                '^' -> { x = j; y = i; dirIdx = 0; found = true }
                '>' -> { x = j; y = i; dirIdx = 1; found = true }
                'v' -> { x = j; y = i; dirIdx = 2; found = true }
                '<' -> { x = j; y = i; dirIdx = 3; found = true }
            }
            if (found) break
        }
        if (found) break
    }

    val visited = mutableSetOf<Pair<Int, Int>>()
    visited.add(x to y)

    while (true) {
        val dir = dirs[dirIdx]
        val nx = x + dir.first
        val ny = y + dir.second
        if (nx < 0 || nx >= w || ny < 0 || ny >= h) break
        if (grid[ny][nx] == '#') {
            dirIdx = (dirIdx + 1) % 4
            continue
        }
        x = nx
        y = ny
        visited.add(x to y)
    }
    println(visited.size)
}
