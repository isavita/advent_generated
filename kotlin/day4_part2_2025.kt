
import java.io.File

fun main() {
    val raw = File("input.txt").readLines()
    if (raw.isEmpty()) {
        println("Total rolls removed: 0")
        return
    }
    val rCount = raw.size
    var cCount = 0
    raw.forEach { if (it.length > cCount) cCount = it.length }
    val grid = Array(rCount) { CharArray(cCount) { '.' } }
    for (i in raw.indices) {
        val line = raw[i]
        for (j in line.indices) grid[i][j] = line[j]
    }
    var removed = 0
    val dr = intArrayOf(-1, -1, -1, 0, 0, 1, 1, 1)
    val dc = intArrayOf(-1, 0, 1, -1, 1, -1, 0, 1)
    var changed: Boolean
    do {
        changed = false
        for (r in 0 until rCount) {
            for (c in 0 until cCount) {
                if (grid[r][c] != '@') continue
                var cnt = 0
                for (k in 0..7) {
                    val nr = r + dr[k]
                    val nc = c + dc[k]
                    if (nr in 0 until rCount && nc in 0 until cCount && grid[nr][nc] == '@') cnt++
                }
                if (cnt < 4) {
                    grid[r][c] = '*'
                    changed = true
                }
            }
        }
        for (r in 0 until rCount) {
            for (c in 0 until cCount) {
                if (grid[r][c] == '*') {
                    grid[r][c] = '.'
                    removed++
                }
            }
        }
    } while (changed)
    println("Total rolls removed: $removed")
}
