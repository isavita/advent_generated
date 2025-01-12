
import java.io.File

data class State(val x: Int, val y: Int, val dir: Int)

fun main() {
    val grid = File("input.txt").readLines().map { it.toCharArray() }
    val h = grid.size
    val w = grid[0].size

    var startX = 0
    var startY = 0
    var startDir = 0
    for (y in 0 until h) {
        for (x in 0 until w) {
            when (grid[y][x]) {
                '^' -> { startX = x; startY = y; startDir = 0 }
                '>' -> { startX = x; startY = y; startDir = 1 }
                'v' -> { startX = x; startY = y; startDir = 2 }
                '<' -> { startX = x; startY = y; startDir = 3 }
            }
        }
    }
    grid[startY][startX] = '.'

    var canLoop = 0
    for (y in 0 until h) {
        for (x in 0 until w) {
            if (x == startX && y == startY) continue
            if (grid[y][x] != '.') continue
            grid[y][x] = '#'
            if (loops(grid, startX, startY, startDir)) {
                canLoop++
            }
            grid[y][x] = '.'
        }
    }
    println(canLoop)
}

fun loops(grid: List<CharArray>, sx: Int, sy: Int, sdir: Int): Boolean {
    val h = grid.size
    val w = grid[0].size
    val dirs = arrayOf(intArrayOf(0, -1), intArrayOf(1, 0), intArrayOf(0, 1), intArrayOf(-1, 0))
    var x = sx
    var y = sy
    var dir = sdir
    val seen = mutableSetOf<State>()
    for (step in 0 until 2000000) {
        val st = State(x, y, dir)
        if (seen.contains(st)) return true
        seen.add(st)
        val dx = dirs[dir][0]
        val dy = dirs[dir][1]
        val nx = x + dx
        val ny = y + dy
        if (nx < 0 || nx >= w || ny < 0 || ny >= h) return false
        if (grid[ny][nx] == '#') {
            dir = (dir + 1) % 4
            continue
        }
        x = nx
        y = ny
    }
    return false
}
