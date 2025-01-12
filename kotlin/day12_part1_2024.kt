
import java.io.File
import java.util.*

data class Point(val x: Int, val y: Int)

fun main() {
    val grid = File("input.txt").readLines()
    val totalPrice = solve(grid)
    println(totalPrice)
}

fun solve(grid: List<String>): Int {
    val rows = grid.size
    if (rows == 0) return 0
    val cols = grid[0].length
    val visited = Array(rows) { BooleanArray(cols) }
    var totalPrice = 0

    for (r in 0 until rows) {
        for (c in 0 until cols) {
            if (!visited[r][c]) {
                val (area, perimeter) = calculateRegion(grid, r, c, visited)
                totalPrice += area * perimeter
            }
        }
    }
    return totalPrice
}

fun calculateRegion(grid: List<String>, row: Int, col: Int, visited: Array<BooleanArray>): Pair<Int, Int> {
    val rows = grid.size
    val cols = grid[0].length
    val char = grid[row][col]
    var area = 0
    var perimeter = 0
    val queue: Queue<Point> = LinkedList()
    queue.add(Point(row, col))
    visited[row][col] = true

    while (queue.isNotEmpty()) {
        val p = queue.poll()
        area++
        val isBorder = p.x == 0 || p.x == rows - 1 || p.y == 0 || p.y == cols - 1

        // Check top
        if (p.x > 0) {
            if (grid[p.x - 1][p.y] != char) {
                perimeter++
            } else if (!visited[p.x - 1][p.y]) {
                queue.add(Point(p.x - 1, p.y))
                visited[p.x - 1][p.y] = true
            }
        } else if (isBorder) {
            perimeter++
        }
        // Check bottom
        if (p.x < rows - 1) {
            if (grid[p.x + 1][p.y] != char) {
                perimeter++
            } else if (!visited[p.x + 1][p.y]) {
                queue.add(Point(p.x + 1, p.y))
                visited[p.x + 1][p.y] = true
            }
        } else if (isBorder) {
            perimeter++
        }
        // Check left
        if (p.y > 0) {
            if (grid[p.x][p.y - 1] != char) {
                perimeter++
            } else if (!visited[p.x][p.y - 1]) {
                queue.add(Point(p.x, p.y - 1))
                visited[p.x][p.y - 1] = true
            }
        } else if (isBorder) {
            perimeter++
        }
        // Check right
        if (p.y < cols - 1) {
            if (grid[p.x][p.y + 1] != char) {
                perimeter++
            } else if (!visited[p.x][p.y + 1]) {
                queue.add(Point(p.x, p.y + 1))
                visited[p.x][p.y + 1] = true
            }
        } else if (isBorder) {
            perimeter++
        }
    }
    return Pair(area, perimeter)
}
