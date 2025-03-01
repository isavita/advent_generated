
import java.io.File

data class Point(val x: Int, val y: Int)

enum class Direction(val points: Int) {
    N(3), E(0), S(1), W(2);

    fun rotate(direction: String): Direction {
        return if (direction == "R") {
            values()[(ordinal + 1) % 4]
        } else {
            values()[(ordinal - 1 + 4) % 4]
        }
    }
}

data class Movement(val steps: Int = 0, val rotate: String? = null)

data class Human(var curr: Point, var facing: Direction)

fun crossBorder(n: Point, dir: Direction, size: Int): Pair<Point, Direction> {
    val x = n.x
    val y = n.y
    val S = size

    return when {
        x == -1 && y < 2 * S -> Point(y + 2 * S, x + 1) to Direction.E
        x == -1 && y >= 2 * S -> Point(x + 4 * S, y - 2 * S) to Direction.N
        x == S && dir == Direction.S -> Point(y - S, x + S - 1) to Direction.W
        x == 2 * S - 1 && dir == Direction.N -> Point(y + S, x - S + 1) to Direction.E
        x == 3 * S && dir == Direction.S -> Point(y + 2 * S, x - 2 * S - 1) to Direction.W
        x == 4 * S -> Point(x - 4 * S, y + 2 * S) to Direction.S
        y == -1 && x < 3 * S -> Point(3 * S - 1 - x, y + S + 1) to Direction.E
        y == -1 && x >= 3 * S -> Point(y + 1, x - 2 * S) to Direction.S
        y == S - 1 && x < S -> Point(3 * S - 1 - x, y - S + 1) to Direction.E
        y == S - 1 && x >= S && dir == Direction.W -> Point(y + S + 1, x - S) to Direction.S
        y == S && dir == Direction.E -> Point(y + 2 * S - 1, x - 2 * S) to Direction.N
        y == 2 * S && x < 2 * S && dir == Direction.E -> Point(y - S - 1, x + S) to Direction.N
        y == 2 * S && x >= 2 * S -> Point(3 * S - 1 - x, y + S - 1) to Direction.W
        y == 3 * S -> Point(3 * S - 1 - x, y - S - 1) to Direction.W
        else -> throw Exception("Not a border crossing")
    }
}

fun parsePath(path: String): List<Movement> {
    val movements = mutableListOf<Movement>()
    var acc = 0
    for (char in path) {
        when (char) {
            'R', 'L' -> {
                if (acc != 0) {
                    movements.add(Movement(steps = acc))
                    acc = 0
                }
                movements.add(Movement(rotate = char.toString()))
            }
            in '0'..'9' -> acc = acc * 10 + (char - '0')
        }
    }
    if (acc != 0) {
        movements.add(Movement(steps = acc))
    }
    return movements
}

fun parseInput(filename: String): Triple<Map<Point, Boolean>, Int, List<Movement>> {
    val mapData = mutableMapOf<Point, Boolean>()
    var size = 0
    var movements: List<Movement> = emptyList()

    File(filename).readLines().let { lines ->
        // Parse map
        var r = 0
        for (line in lines) {
            if (line.isEmpty()) break
            if (r == 0) size = line.length / 3
            for ((c, char) in line.withIndex()) {
                when (char) {
                    ' ' -> continue
                    '#' -> mapData[Point(r, c)] = true
                    '.' -> mapData[Point(r, c)] = false
                }
            }
            r++
        }

        // Parse movements
        movements = parsePath(lines[r + 1].trim())
    }

    return Triple(mapData, size, movements)
}

fun main() {
    val (mapData, size, movements) = parseInput("input.txt")
    val dirs = listOf(
        Point(-1, 0), // N
        Point(0, 1),  // E
        Point(1, 0),  // S
        Point(0, -1)  // W
    )

    var human = Human(Point(0, size), Direction.E)

    for (mov in movements) {
        if (mov.rotate != null) {
            human.facing = human.facing.rotate(mov.rotate)
        }
        repeat(mov.steps) {
            val dirDelta = dirs[human.facing.ordinal]
            val nextPos = Point(human.curr.x + dirDelta.x, human.curr.y + dirDelta.y)
            if (nextPos in mapData) {
                if (mapData[nextPos]!!) {
                    return@repeat // Hit a wall
                } else {
                    human.curr = nextPos
                    human.facing = human.facing
                }
            } else {
                val (newPos, newFacing) = crossBorder(nextPos, human.facing, size)
                if (mapData.getOrDefault(newPos, true)) {
                    return@repeat // Hit a wall after crossing
                }
                human.curr = newPos
                human.facing = newFacing
            }
        }
    }

    val finalValue = 1000 * (human.curr.x + 1) + 4 * (human.curr.y + 1) + human.facing.points
    println(finalValue)
}
