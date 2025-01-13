
import java.io.File
import java.util.PriorityQueue

data class Coord(val x: Int, val y: Int) {
    operator fun plus(other: Coord) = Coord(x + other.x, y + other.y)
    operator fun minus(other: Coord) = Coord(x - other.x, y - other.y)
    fun opposite() = Coord(-x, -y)
}

data class Grid(val width: Int, val height: Int, val data: Map<Coord, Int>) {
    fun neighbors4(coord: Coord): List<Coord> {
        val directions = listOf(Coord(0, -1), Coord(-1, 0), Coord(0, 1), Coord(1, 0))
        return directions.map { coord + it }.filter { it.x in 0 until width && it.y in 0 until height }
    }
}

data class Info(val coord: Coord, val dir: Coord, val numStraight: Int)

fun heuristic(c1: Coord, c2: Coord) = Math.abs(c1.x - c2.x) + Math.abs(c1.y - c2.y)

fun Grid.aStarConstrained(start: Coord, goal: Coord, minStraight: Int, maxStraight: Int): Int {
    val startInfo = Info(start, Coord(0, 0), 0)
    val frontier = PriorityQueue<Pair<Info, Int>>(compareBy { it.second })
    frontier.add(startInfo to 0)

    val cameFrom = mutableMapOf<Info, Info>()
    val costSoFar = mutableMapOf<Info, Int>()
    cameFrom[startInfo] = startInfo
    costSoFar[startInfo] = 0

    while (frontier.isNotEmpty()) {
        val (current, _) = frontier.poll()

        if (current.coord == goal) {
            return costSoFar[current]!!
        }

        for (next in neighbors4(current.coord)) {
            val newDir = next - current.coord
            val newNumStraight = if (newDir == current.dir) current.numStraight + 1 else 1

            val nextInfo = Info(next, newDir, newNumStraight)
            val newCost = costSoFar[current]!! + data[next]!!

            val isLowerCost = nextInfo !in costSoFar || newCost < costSoFar[nextInfo]!!
            val isValidStraight = (current.numStraight >= minStraight || newDir == current.dir || current.coord == start) &&
                    (newNumStraight <= maxStraight)
            val isNotOppositeDirection = newDir != current.dir.opposite()

            if (isLowerCost && isValidStraight && isNotOppositeDirection) {
                costSoFar[nextInfo] = newCost
                cameFrom[nextInfo] = current

                val priority = newCost + heuristic(next, goal)
                frontier.add(nextInfo to priority)
            }
        }
    }

    return -1
}

fun solve(input: List<String>): Int {
    val gridData = input.flatMapIndexed { y, line ->
        line.mapIndexed { x, char -> Coord(x, y) to char.digitToInt() }
    }.toMap()
    val grid = Grid(input[0].length, input.size, gridData)

    val start = Coord(0, 0)
    val goal = Coord(grid.width - 1, grid.height - 1)
    return grid.aStarConstrained(start, goal, 4, 10)
}

fun main() {
    val input = File("input.txt").readLines()
    println(solve(input))
}
