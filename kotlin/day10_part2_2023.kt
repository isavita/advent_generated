import java.io.File

data class Coord(val x: Int, val y: Int) {
    fun add(c: Coord) = Coord(x + c.x, y + c.y)
    fun substract(c: Coord) = Coord(x - c.x, y - c.y)
    fun opposite() = Coord(-x, -y)
}

typealias Pipe = Map<Coord, Unit>
typealias Tile = Char

data class Grid(val width: Int, val height: Int, val data: Map<Coord, Tile>)

val undefined = Coord(0, 0)
val top = Coord(0, -1)
val right = Coord(1, 0)
val bottom = Coord(0, 1)
val left = Coord(-1, 0)

const val empty = '.'
const val start = 'S'
const val vertical = '|'
const val horizontal = '-'
const val topLeftCorner = 'J'
const val topRightCorner = 'L'
const val bottomLeftCorner = '7'
const val bottomRightCorner = 'F'
const val enclosed = 'X'

val verticalPipe = mapOf(top to Unit, bottom to Unit)
val horizontalPipe = mapOf(left to Unit, right to Unit)
val topLeftCornerPipe = mapOf(top to Unit, left to Unit)
val topRightCornerPipe = mapOf(top to Unit, right to Unit)
val bottomLeftCornerPipe = mapOf(bottom to Unit, left to Unit)
val bottomRightCornerPipe = mapOf(bottom to Unit, right to Unit)

val tileToPipe = mapOf(
    vertical to verticalPipe,
    horizontal to horizontalPipe,
    topLeftCorner to topLeftCornerPipe,
    topRightCorner to topRightCornerPipe,
    bottomLeftCorner to bottomLeftCornerPipe,
    bottomRightCorner to bottomRightCornerPipe
)

fun getPipeFromTile(tile: Tile) = tileToPipe[tile] ?: emptyMap()

fun getTileFromPipe(pipe: Pipe): Tile {
    for ((tile, associatedPipe) in tileToPipe) {
        if (pipe == associatedPipe) {
            return tile
        }
    }
    return empty
}

fun Pipe.isEqualPipe(pipe2: Pipe) = this == pipe2

fun buildGrid(input: List<String>): Grid {
    val data = mutableMapOf<Coord, Tile>()
    input.forEachIndexed { y, line ->
        line.forEachIndexed { x, char ->
            if (char != empty) {
                data[Coord(x, y)] = char
            }
        }
    }
    return Grid(input[0].length, input.size, data)
}

fun Grid.toStringRepresentation(): String {
    val pipesRepres = mapOf(
        empty to " ",
        start to "S",
        vertical to "║",
        horizontal to "═",
        topLeftCorner to "╝",
        topRightCorner to "╚",
        bottomLeftCorner to "╗",
        bottomRightCorner to "╔",
        enclosed to "X"
    )

    val sb = StringBuilder()
    for (y in 0 until height) {
        for (x in 0 until width) {
            val coord = Coord(x, y)
            sb.append(pipesRepres[data[coord] ?: empty])
        }
        sb.append("\n")
    }
    return sb.toString()
}

fun findStart(grid: Grid): Coord {
    grid.data.entries.forEach { (coord, value) ->
        if (value == start) {
            return coord
        }
    }
    return undefined
}

fun Coord.getPipeFromNeighbors(grid: Grid): Pipe {
    val pipe = mutableMapOf<Coord, Unit>()

    val possibleNeighbors = mapOf(
        top to add(top),
        right to add(right),
        bottom to add(bottom),
        left to add(left)
    )

    for ((dir, neighborCoord) in possibleNeighbors) {
        val neighborPipe = getPipeFromTile(grid.data[neighborCoord] ?: empty)
        if (neighborPipe.containsKey(dir.opposite())) {
            pipe[dir] = Unit
        }
    }

    return pipe
}

fun Coord.pathFinding(grid: Grid): List<Coord> {
    val path = mutableListOf(this)
    var previousDir: Coord
    var current: Coord
    val startPipe = getPipeFromNeighbors(grid)

    val (dir, neighborCoord) = startPipe.entries.first()
    previousDir = dir
    current = add(dir)

    while (current != this) {
        path.add(current)
        val currentPipe = getPipeFromTile(grid.data[current] ?: empty)
        for ((dir, _) in currentPipe) {
            if (dir != previousDir.opposite()) {
                previousDir = dir
                current = current.add(dir)
                break
            }
        }
    }

    return path
}

fun getPathGrid(grid: Grid, path: List<Coord>, empty: Tile): Grid {
    val data = mutableMapOf<Coord, Tile>()
    path.forEach { coord ->
        data[coord] = grid.data[coord] ?: empty
    }

    val start = path[0]
    data[start] = getTileFromPipe(start.getPipeFromNeighbors(grid))

    return Grid(grid.width, grid.height, data)
}

fun Coord.isInside(grid: Grid, empty: Tile): Boolean {
    if (grid.data.containsKey(this)) {
        return false
    }

    var startPipe = empty
    var numPipeOnLeft = 0
    for (x in 0 until x) {
        val coord = Coord(x, y)
        val v = grid.data[coord] ?: empty

        when (v) {
            vertical -> numPipeOnLeft++
            topRightCorner -> startPipe = topRightCorner
            bottomRightCorner -> startPipe = bottomRightCorner
            topLeftCorner -> {
                if (startPipe == bottomRightCorner) {
                    startPipe = empty
                    numPipeOnLeft++
                } else if (v == topRightCorner) {
                    startPipe = empty
                }
            }
            bottomLeftCorner -> {
                if (startPipe == topRightCorner) {
                    startPipe = empty
                    numPipeOnLeft++
                } else if (startPipe == bottomRightCorner) {
                    startPipe = empty
                }
            }
        }
    }

    return numPipeOnLeft % 2 == 1
}

fun solve(input: List<String>): Int {
    val grid = buildGrid(input)
    val start = findStart(grid)
    val path = start.pathFinding(grid)
    val pathGrid = getPathGrid(grid, path, empty)

    var count = 0
    for (y in 0 until grid.height) {
        for (x in 0 until grid.width) {
            val coord = Coord(x, y)
            if (coord.isInside(pathGrid, empty)) {
                count++
            }
        }
    }

    return count
}

fun main() {
    val input = File("input.txt").readLines()
    println(solve(input))
}