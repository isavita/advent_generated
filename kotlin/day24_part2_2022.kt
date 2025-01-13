
import java.awt.Point
import java.awt.Rectangle
import java.io.File

data class State(val pos: Point, val step: Int)

fun main() {
    val grid = mutableMapOf<Point, Char>()
    val input = File("input.txt").readText().trim()
    val lines = input.split("\n")
    for ((y, line) in lines.withIndex()) {
        for (x in line.indices) {
            if (line[x] != '.') {
                grid[Point(x, y)] = line[x]
            }
        }
    }

    val bounds = bounds(grid.keys)
    val entrance = Point(1, 0)
    val exit = Point(bounds.width - 2, bounds.height - 1)
    val firstCrossing = steps(grid, bounds, entrance, exit, 0)
    val secondCrossing = steps(grid, bounds, exit, entrance, firstCrossing)
    val thirdCrossing = steps(grid, bounds, entrance, exit, secondCrossing)

    println(thirdCrossing)
}

val neighbors4 = listOf(Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0))

fun steps(grid: Map<Point, Char>, bounds: Rectangle, start: Point, end: Point, initialStep: Int): Int {
    val q = ArrayDeque<State>()
    q.add(State(start, initialStep))
    val seen = mutableSetOf<State>()
    val width = bounds.width - 2
    val height = bounds.height - 2
    val lcm = lcm(width, height)

    while (q.isNotEmpty()) {
        val curr = q.removeFirst()
        if (curr.pos == end) {
            return curr.step
        }

        for (n in neighbors4 + Point(0, 0)) {
            val nextPos = Point(curr.pos.x + n.x, curr.pos.y + n.y)
            val nextStep = curr.step + 1
            val nextState = State(nextPos, nextStep)

            if (nextState in seen) continue
            if (!bounds.contains(nextPos)) continue
            if (grid[nextPos] == '#') continue

            var valid = true
            if (nextPos.y in 1 until bounds.height - 1) {
                for (bliz in listOf('^', '>', 'v', '<')) {
                    val dir = dirFromChar(bliz)
                    val prevX = (nextPos.x - 1 - dir.pointR.x * nextStep).mod(width) + 1
                    val prevY = (nextPos.y - 1 - dir.pointR.y * nextStep).mod(height) + 1
                    if (grid[Point(prevX, prevY)] == bliz) {
                        valid = false
                        break
                    }
                }
            }

            if (valid) {
                if (nextStep % lcm == 0) seen.clear()
                q.add(nextState)
                seen.add(nextState)
            }
        }
    }
    return -1
}

fun bounds(p: Set<Point>): Rectangle {
    var minX = Int.MAX_VALUE
    var minY = Int.MAX_VALUE
    var maxX = Int.MIN_VALUE
    var maxY = Int.MIN_VALUE
    for (pp in p) {
        minX = minOf(minX, pp.x)
        minY = minOf(minY, pp.y)
        maxX = maxOf(maxX, pp.x)
        maxY = maxOf(maxY, pp.y)
    }
    return Rectangle(minX, minY, maxX - minX + 1, maxY - minY + 1)
}

enum class Dir {
    N, E, S, W
}

val Dir.point: Point
    get() = when (this) {
        Dir.N -> Point(0, 1)
        Dir.E -> Point(1, 0)
        Dir.S -> Point(0, -1)
        Dir.W -> Point(-1, 0)
    }

val Dir.pointR: Point
    get() = when (this) {
        Dir.N -> Point(0, -1)
        Dir.E -> Point(1, 0)
        Dir.S -> Point(0, 1)
        Dir.W -> Point(-1, 0)
    }

fun dirFromChar(c: Char): Dir = when (c) {
    '^' -> Dir.N
    '>' -> Dir.E
    'v' -> Dir.S
    '<' -> Dir.W
    else -> error("Invalid direction")
}

fun gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
fun lcm(a: Int, b: Int): Int = a / gcd(a, b) * b
