
import java.io.File

fun gcd(a: Int, b: Int): Int = if (b == 0) if (a < 0) -a else a else gcd(b, a % b)

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

    val linesPerFreq = mutableMapOf<Char, MutableSet<String>>()
    for ((f, coords) in antennas) {
        linesPerFreq[f] = mutableSetOf()
        for (i in 0 until coords.size) {
            for (j in i + 1 until coords.size) {
                val a = coords[i]
                val b = coords[j]
                var dy = b.first - a.first
                var dx = b.second - a.second
                val g = gcd(dy, dx)
                var sy = dy / g
                var sx = dx / g
                if (sx < 0 || (sx == 0 && sy < 0)) {
                    sx = -sx
                    sy = -sy
                }
                val c = sy * a.second - sx * a.first
                linesPerFreq[f]!!.add("$sx,$sy,$c")
            }
        }
    }

    val antinodes = mutableSetOf<Pair<Int, Int>>()
    for (lines in linesPerFreq.values) {
        for (key in lines) {
            val (sx, sy, c) = key.split(",").map { it.toInt() }
            if (sx == 0 && sy == 0) continue
            if (sy == 0) {
                if (c % sx == 0) {
                    val y = -c / sx
                    if (y in 0 until h) {
                        for (x in 0 until w) antinodes.add(Pair(y, x))
                    }
                }
            } else if (sx == 0) {
                if (c % sy == 0) {
                    val x = c / sy
                    if (x in 0 until w) {
                        for (y in 0 until h) antinodes.add(Pair(y, x))
                    }
                }
            } else {
                for (y in 0 until h) {
                    val value = c + sx * y
                    if (value % sy == 0) {
                        val x = value / sy
                        if (x in 0 until w) antinodes.add(Pair(y, x))
                    }
                }
            }
        }
    }

    println(antinodes.size)
}
