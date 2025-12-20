
import java.io.File

fun main() {
    val lines = File("input.txt").readLines()
    if (lines.isEmpty()) return
    val height = lines.size
    val width = lines[0].length
    val grid = Array(height) { lines[it].toCharArray() }

    var sx = 0
    var sy = 0
    outer@ for (y in 0 until height) {
        for (x in 0 until width) {
            if (grid[y][x] == 'S') {
                sx = x
                sy = y
                break@outer
            }
        }
    }

    var splits = 0
    var active = BooleanArray(width)
    var next = BooleanArray(width)
    active[sx] = true

    for (y in sy until height) {
        java.util.Arrays.fill(next, false)
        for (x in 0 until width) {
            if (!active[x]) continue
            if (grid[y][x] == '^') {
                splits++
                if (x > 0) next[x - 1] = true
                if (x + 1 < width) next[x + 1] = true
            } else {
                next[x] = true
            }
        }
        val any = next.any { it }
        if (!any) break
        val tmp = active
        active = next
        next = tmp
    }

    println(splits)
}
