import java.io.File

data class Coord(val x: Int, val y: Int) {
    fun add(c2: Coord) = Coord(x + c2.x, y + c2.y)
}

data class Grid(val width: Int, val height: Int, val data: Map<Coord, Char>)

val North = Coord(0, -1)
val West = Coord(-1, 0)
val South = Coord(0, 1)
val East = Coord(1, 0)

const val Empty = '.'
const val Rock = '#'
const val Start = 'S'

fun Grid.toString(): String {
    var res = ""
    for (y in 0 until height) {
        for (x in 0 until width) {
            res += data[Coord(x, y)] ?: Empty
        }
        res += "\n"
    }
    return res
}

fun isInBounds(grid: Grid, coord: Coord) =
    coord.x in 0 until grid.width && coord.y in 0 until grid.height

fun parseInput(input: List<String>): Grid {
    val data = mutableMapOf<Coord, Char>()
    input.forEachIndexed { y, line ->
        line.forEachIndexed { x, char ->
            if (char != Empty) {
                data[Coord(x, y)] = char
            }
        }
    }
    return Grid(input[0].length, input.size, data)
}

fun findStart(grid: Grid): Coord =
    grid.data.entries.first { it.value == Start }.key

fun neighbors4(grid: Grid, coord: Coord) =
    listOf(coord.add(North), coord.add(South), coord.add(East), coord.add(West))
        .filter { isInBounds(grid, it) && grid.data[it] != Rock }

fun breadthFirstSearch(grid: Grid, start: Coord, neighborFunc: (Grid, Coord) -> List<Coord>): Map<Coord, Int> {
    val frontier = mutableListOf(start)
    val reached = mutableSetOf(start)
    val cameFrom = mutableMapOf(start to start)
    val distances = mutableMapOf(start to 0)

    while (frontier.isNotEmpty()) {
        val current = frontier.removeAt(0)
        neighborFunc(grid, current).forEach { next ->
            if (next !in reached) {
                frontier.add(next)
                reached.add(next)
                cameFrom[next] = current
                distances[next] = distances[current]!! + 1
            }
        }
    }

    return distances
}

fun distancesFromExtremities(grid: Grid): Map<Coord, Map<Coord, Int>> {
    val distances = mutableMapOf<Coord, Map<Coord, Int>>()

    val extremities = listOf(
        Coord(0, 0), Coord(grid.width / 2, 0), Coord(grid.width, 0),
        Coord(grid.width, grid.height / 2), Coord(grid.width, grid.height),
        Coord(grid.width / 2, grid.height), Coord(0, grid.height),
        Coord(0, grid.height / 2)
    )

    extremities.forEach { start ->
        distances[start] = breadthFirstSearch(grid, start, ::neighbors4)
    }

    return distances
}

fun neighbors8(grid: Grid, coord: Coord) =
    listOf(
        coord.add(North), coord.add(South), coord.add(East), coord.add(West),
        coord.add(North).add(East), coord.add(North).add(West),
        coord.add(South).add(East), coord.add(South).add(West)
    )

fun solve(input: List<String>, numSteps: Int): Int {
    val grid = parseInput(input)
    val start = findStart(grid)
    val distances = breadthFirstSearch(grid, start, ::neighbors4)

    return distances.count { it.value <= numSteps && it.value % 2 == 0 }
}

fun main() {
    val input = File("input.txt").readLines()
    println(solve(input, 64))
}