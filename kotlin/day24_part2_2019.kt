
import java.io.File

const val SIDE = 5
const val SQUARE = SIDE * SIDE

fun parse(): Array<Boolean> {
    val res = Array(SQUARE) { false }
    File("input.txt").readLines().forEachIndexed { row, line ->
        line.forEachIndexed { col, c ->
            res[row * SIDE + col] = c == '#'
        }
    }
    return res
}

typealias Space = MutableMap<Int, Array<Boolean>>

fun main() {
    val input = parse()
    val space: Space = mutableMapOf(0 to input)

    repeat(200) {
        space.next2()
    }

    var count = 0
    for (grid in space.values) {
        for (i in 0 until SQUARE) {
            if (grid[i]) {
                count++
            }
        }
    }
    println(count)
}

fun Space.next2() {
    val newSpace: Space = mutableMapOf()
    val (minLevel, maxLevel) = minMaxLevel()

    for (level in minLevel - 1..maxLevel + 1) {
        newSpace[level] = Array(SQUARE) { false }

        for (cell in 0 until SQUARE) {
            if (cell == 12) continue

            val row = cell / SIDE
            val col = cell % SIDE
            var neighbours = 0

            if (row == 0 && infested(level - 1, 7)) neighbours++
            if (col == 0 && infested(level - 1, 11)) neighbours++
            if (col == 4 && infested(level - 1, 13)) neighbours++
            if (row == 4 && infested(level - 1, 17)) neighbours++

            if (cell == 7) {
                for (i in 0 until SIDE) if (infested(level + 1, i)) neighbours++
            }
            if (cell == 11) {
                for (i in 0 until SIDE) if (infested(level + 1, 5 * i)) neighbours++
            }
            if (cell == 13) {
                for (i in 0 until SIDE) if (infested(level + 1, 5 * i + SIDE - 1)) neighbours++
            }
            if (cell == 17) {
                for (i in 0 until SIDE) if (infested(level + 1, (SIDE - 1) * SIDE + i)) neighbours++
            }

            if (row > 0 && cell != 17 && infested(level, cell - SIDE)) neighbours++
            if (col > 0 && cell != 13 && infested(level, cell - 1)) neighbours++
            if (col < SIDE - 1 && cell != 11 && infested(level, cell + 1)) neighbours++
            if (row < SIDE - 1 && cell != 7 && infested(level, cell + SIDE)) neighbours++

            newSpace[level]!![cell] = when {
                infested(level, cell) && neighbours != 1 -> false
                !infested(level, cell) && (neighbours == 1 || neighbours == 2) -> true
                else -> infested(level, cell)
            }
        }
    }

    newSpace.clean()
    this.clear()
    this.putAll(newSpace)
}

fun Space.clean() {
    val (min, max) = minMaxLevel()
    if (this[min]?.all { !it } == true) remove(min)
    if (this[max]?.all { !it } == true) remove(max)
}

fun Space.infested(level: Int, cell: Int): Boolean {
    return this[level]?.get(cell) ?: false
}

fun Space.minMaxLevel(): Pair<Int, Int> {
    var min = Int.MAX_VALUE
    var max = Int.MIN_VALUE
    for (level in keys) {
        min = minOf(min, level)
        max = maxOf(max, level)
    }
    return min to max
}
