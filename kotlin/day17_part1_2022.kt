import java.io.File

const val rockStr = """
####

 #
###
 #

  #
  #
###

#
#
#
#

##
##
"""

fun main() {
    val jetPattern = File("input.txt").readText().trim()
    val rocks = getRocks(rockStr)
    val grid = mutableMapOf<Pair<Int, Int>, Unit>()
    for (x in 0 until 7) {
        grid[x to 0] = Unit
    }
    var floor = 0
    var j = 0
    var curr = 0 // Initialize curr variable

    for (i in 0 until Int.MAX_VALUE) {
        if (i == 2022) {
            println(floor)
            break
        }
        val key = curr to j
        val currRock = rocks[curr]
        var pos = 2 to floor + 4
        while (true) {
            val jet = jetPattern[j]
            j = (j + 1) % jetPattern.length
            pos = move(pos, dirFromByte(jet))
            if (collision(grid, currRock, pos)) {
                pos = move(pos, dirFromByte(jet).reverse())
            }
            pos = move(pos, Dir.S)
            if (collision(grid, currRock, pos)) {
                pos = move(pos, Dir.S.reverse())
                for (p in currRock) {
                    grid[p.add(pos)] = Unit
                    if (p.add(pos).second > floor) {
                        floor = p.add(pos).second
                    }
                }
                break
            }
        }
        curr = (curr + 1) % rocks.size // Update curr to cycle through rocks
    }
}

fun collision(grid: Map<Pair<Int, Int>, Unit>, rock: Set<Pair<Int, Int>>, pos: Pair<Int, Int>): Boolean {
    for (p in rock) {
        val newPos = p.add(pos)
        if (newPos in grid || newPos.first < 0 || newPos.first > 6 || newPos.second < 0) {
            return true
        }
    }
    return false
}

fun getRocks(rockStr: String): List<Set<Pair<Int, Int>>> {
    val rocks = mutableListOf<Set<Pair<Int, Int>>>()
    for (rock in rockStr.trim().split("\n\n")) {
        val rockSet = mutableSetOf<Pair<Int, Int>>()
        val lines = rock.split("\n")
        for ((y, line) in lines.withIndex()) {
            for ((x, char) in line.withIndex()) {
                if (char == '#') {
                    rockSet.add(x to lines.size - 1 - y)
                }
            }
        }
        rocks.add(rockSet)
    }
    return rocks
}

fun move(pos: Pair<Int, Int>, dir: Dir): Pair<Int, Int> {
    return pos.first + dir.dx to pos.second + dir.dy
}

fun Pair<Int, Int>.add(other: Pair<Int, Int>): Pair<Int, Int> {
    return first + other.first to second + other.second
}

enum class Dir(val dx: Int, val dy: Int) {
    N(0, 1), E(1, 0), S(0, -1), W(-1, 0);

    fun reverse() = when (this) {
        N -> S
        E -> W
        S -> N
        W -> E
    }
}

fun dirFromByte(b: Char): Dir {
    return when (b) {
        '>', 'R' -> Dir.E
        '<', 'L' -> Dir.W
        else -> throw IllegalArgumentException("Invalid direction byte: $b")
    }
}