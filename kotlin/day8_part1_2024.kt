
import java.io.File

fun main() {
    val grid = File("input.txt").readLines()
    val h = grid.size
    val w = grid[0].length
    val antennas = mutableMapOf<Char, MutableList<Pair<Int, Int>>>()

    for (y in 0 until h) {
        for (x in 0 until w) {
            val c = grid[y][x]
            if (c != '.') {
                antennas.getOrPut(c) { mutableListOf() }.add(Pair(y, x))
            }
        }
    }

    val antinodes = mutableSetOf<Pair<Int, Int>>()
    for (coords in antennas.values) {
        val n = coords.size
        for (i in 0 until n) {
            for (j in i + 1 until n) {
                val (ay, ax) = coords[i]
                val (by, bx) = coords[j]
                val p1y = 2 * ay - by
                val p1x = 2 * ax - bx
                val p2y = 2 * by - ay
                val p2x = 2 * bx - ax

                if (p1y in 0 until h && p1x in 0 until w) {
                    antinodes.add(Pair(p1y, p1x))
                }
                if (p2y in 0 until h && p2x in 0 until w) {
                    antinodes.add(Pair(p2y, p2x))
                }
            }
        }
    }
    println(antinodes.size)
}
