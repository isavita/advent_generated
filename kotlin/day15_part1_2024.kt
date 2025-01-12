
import java.io.File

fun main() {
    val lines = File("input.txt").readLines()
    val grid = lines.takeWhile { it.contains('#') }.map { it.toCharArray() }
    val moves = lines.dropWhile { it.contains('#') }.joinToString("")

    var robotR = 0
    var robotC = 0
    for (r in grid.indices) {
        for (c in grid[r].indices) {
            if (grid[r][c] == '@') {
                robotR = r
                robotC = c
                break
            }
        }
    }

    val dirs = mapOf(
        '^' to Pair(-1, 0),
        'v' to Pair(1, 0),
        '<' to Pair(0, -1),
        '>' to Pair(0, 1)
    )

    for (move in moves) {
        val (dr, dc) = dirs[move]!!
        val nr = robotR + dr
        val nc = robotC + dc

        if (grid[nr][nc] == '#') continue
        if (grid[nr][nc] == 'O') {
            if (!pushBoxes(grid, nr, nc, dr, dc)) continue
        }
        if (grid[nr][nc] == '.' || grid[nr][nc] == 'O') {
            grid[robotR][robotC] = '.'
            grid[nr][nc] = '@'
            robotR = nr
            robotC = nc
        }
    }

    var sum = 0
    for (r in grid.indices) {
        for (c in grid[r].indices) {
            if (grid[r][c] == 'O') {
                sum += r * 100 + c
            }
        }
    }
    println(sum)
}

fun pushBoxes(grid: List<CharArray>, r: Int, c: Int, dr: Int, dc: Int): Boolean {
    val nr = r + dr
    val nc = c + dc
    if (grid[nr][nc] == '#') return false
    if (grid[nr][nc] == 'O') {
        if (!pushBoxes(grid, nr, nc, dr, dc)) return false
    }
    if (grid[nr][nc] == '.') {
        grid[nr][nc] = 'O'
        grid[r][c] = '.'
        return true
    }
    return false
}
