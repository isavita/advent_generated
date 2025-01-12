
import java.awt.Point
import java.io.File

private const val rockstr = """####

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
##"""

fun main() {
    val jetPattern = File("input.txt").readText().trim().toByteArray()
    val rocks = getRocks()
    val grid = mutableSetOf<Point>()
    for (x in 0..6) {
        grid.add(Point(x, 0))
    }
    var floor = 0
    var j = 0
    val repeat = mutableMapOf<Pair<Int, Int>, Pair<Int, Int>>()

    var i = 0
    var curr = 0
    while (true) {
        val key = curr to j
        if (repeat.containsKey(key)) {
            val (previ, prevFloor) = repeat[key]!!
            if ((1000000000000L - i) % (i - previ) == 0L) {
                println(floor + (1000000000000L - i) / (i - previ) * (floor - prevFloor))
                break
            }
        }
        repeat[key] = i to floor
        val currRock = rocks[curr]
        var pos = Point(2, floor + 4)
        while (true) {
            val jet = jetPattern[j].toInt().toChar()
            j = (j + 1) % jetPattern.size
            pos = pos.add(dirFromByte(jet).point())
            if (collision(grid, currRock, pos)) {
                pos = pos.subtract(dirFromByte(jet).point())
            }
            pos = pos.add(S.point())
            if (collision(grid, currRock, pos)) {
                pos = pos.subtract(S.point())
                for (p in currRock) {
                    val newP = p.add(pos)
                    grid.add(newP)
                    if (newP.y > floor) {
                        floor = newP.y
                    }
                }
                break
            }
        }
        i++
        curr = (curr + 1) % rocks.size
    }
}

private fun collision(grid: Set<Point>, rock: Set<Point>, pos: Point): Boolean {
    for (p in rock) {
        val newP = p.add(pos)
        if (grid.contains(newP) || newP.x < 0 || newP.x > 6) {
            return true
        }
    }
    return false
}

private fun getRocks(): List<Set<Point>> {
    return rockstr.split("\n\n").map { rock ->
        val lines = rock.split("\n")
        val points = mutableSetOf<Point>()
        for (y in lines.indices) {
            for (x in lines[y].indices) {
                if (lines[y][x] == '#') {
                    points.add(Point(x, lines.size - 1 - y))
                }
            }
        }
        points
    }
}

private enum class Dir {
    N, E, S, W;

    fun point(): Point {
        return when (this) {
            N -> Point(0, 1)
            E -> Point(1, 0)
            S -> Point(0, -1)
            W -> Point(-1, 0)
        }
    }
}

private val S = Dir.S

private fun dirFromByte(b: Char): Dir {
    return when (b) {
        'N', 'U', '^' -> Dir.N
        'E', 'R', '>' -> Dir.E
        'S', 'D', 'v' -> Dir.S
        'W', 'L', '<' -> Dir.W
        else -> throw IllegalArgumentException("Invalid direction: $b")
    }
}

private fun Point.add(other: Point): Point {
    return Point(this.x + other.x, this.y + other.y)
}

private fun Point.subtract(other: Point): Point {
    return Point(this.x - other.x, this.y - other.y)
}
