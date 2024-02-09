import java.io.File

data class Coord(val x: Int, val y: Int) {
    fun add(c: Coord) = Coord(x + c.x, y + c.y)
}

data class Beam(val origin: Coord, val dir: Coord)

data class Grid(val width: Int, val height: Int, val data: MutableMap<Coord, Char>)

val empty = '.'
val ascendingMirror = '/'
val descendingMirror = '\\'
val verticalSplitter = '|'
val horizontalSplitter = '-'

val north = Coord(0, -1)
val west = Coord(-1, 0)
val south = Coord(0, 1)
val east = Coord(1, 0)

fun Coord.rotate90() = Coord(y, -x)
fun Coord.rotateNeg90() = Coord(-y, x)
fun Coord.isInBounds(grid: Grid) = x in 0 until grid.width && y in 0 until grid.height

fun buildGrid(input: List<String>): Grid {
    val grid = Grid(input[0].length, input.size, mutableMapOf())

    input.forEachIndexed { y, line ->
        line.forEachIndexed { x, char ->
            if (char != empty) {
                grid.data[Coord(x, y)] = char
            }
        }
    }

    return grid
}

fun Grid.toString(): String {
    var result = ""

    repeat(height) { y ->
        repeat(width) { x ->
            val coord = Coord(x, y)
            result += data[coord] ?: empty
        }
        result += "\n"
    }

    return result
}

fun nextBeam(grid: Grid, beam: Beam): List<Beam> {
    val beams = mutableListOf<Beam>()
    val char = grid.data[beam.origin]

    if (char == null) {
        val newBeam = Beam(beam.origin.add(beam.dir), beam.dir)
        return listOf(newBeam)
    }

    when {
        char == ascendingMirror -> {
            val newDir = if (beam.dir == north || beam.dir == south) beam.dir.rotateNeg90() else beam.dir.rotate90()
            val newBeam = Beam(beam.origin.add(newDir), newDir)
            beams.add(newBeam)
        }
        char == descendingMirror -> {
            val newDir = if (beam.dir == north || beam.dir == south) beam.dir.rotate90() else beam.dir.rotateNeg90()
            val newBeam = Beam(beam.origin.add(newDir), newDir)
            beams.add(newBeam)
        }
        char == verticalSplitter && (beam.dir == east || beam.dir == west) -> {
            val newDir1 = beam.dir.rotate90()
            val newBeam1 = Beam(beam.origin.add(newDir1), newDir1)
            val newDir2 = beam.dir.rotateNeg90()
            val newBeam2 = Beam(beam.origin.add(newDir2), newDir2)
            beams.addAll(listOf(newBeam1, newBeam2))
        }
        char == horizontalSplitter && (beam.dir == north || beam.dir == south) -> {
            val newDir1 = beam.dir.rotate90()
            val newBeam1 = Beam(beam.origin.add(newDir1), newDir1)
            val newDir2 = beam.dir.rotateNeg90()
            val newBeam2 = Beam(beam.origin.add(newDir2), newDir2)
            beams.addAll(listOf(newBeam1, newBeam2))
        }
        else -> {
            val newBeam = Beam(beam.origin.add(beam.dir), beam.dir)
            beams.add(newBeam)
        }
    }

    return beams
}

fun calculatePropagation(grid: Grid, start: Beam): Set<Beam> {
    val alreadySeen = mutableSetOf<Beam>()
    val toExplore = mutableListOf(start)

    while (toExplore.isNotEmpty()) {
        val beam = toExplore.removeAt(0)
        val isAlreadySeen = alreadySeen.contains(beam)
        if (beam.origin.isInBounds(grid) && !isAlreadySeen) {
            alreadySeen.add(beam)
            toExplore.addAll(nextBeam(grid, beam))
        }
    }

    return alreadySeen
}

fun buildBeamGrid(grid: Grid, alreadySeen: Set<Beam>): Grid {
    val beamGrid = Grid(grid.width, grid.height, mutableMapOf())

    grid.data.forEach { (coord, char) ->
        beamGrid.data[coord] = char
    }

    alreadySeen.forEach { beam ->
        if (grid.data[beam.origin] == null) {
            if (beamGrid.data[beam.origin] != null) {
                beamGrid.data[beam.origin] = '2'
            } else {
                val char = when (beam.dir) {
                    north -> '^'
                    east -> '>'
                    south -> 'v'
                    west -> '<'
                    else -> '?'
                }
                beamGrid.data[beam.origin] = char
            }
        }
    }

    return beamGrid
}

fun calculateEnergization(alreadySeen: Set<Beam>): Set<Coord> {
    val alreadyEnergized = mutableSetOf<Coord>()

    alreadySeen.forEach { beam ->
        alreadyEnergized.add(beam.origin)
    }

    return alreadyEnergized
}

fun getBorder(grid: Grid): List<Beam> {
    val border = mutableListOf<Beam>()

    repeat(grid.width) { x ->
        val coord = Coord(x, 0)
        border.add(Beam(coord, south))

        val coord2 = Coord(x, grid.height - 1)
        border.add(Beam(coord2, north))
    }

    repeat(grid.height) { y ->
        val coord = Coord(0, y)
        border.add(Beam(coord, east))

        val coord2 = Coord(grid.width - 1, y)
        border.add(Beam(coord2, west))
    }

    return border
}

fun solve(input: List<String>): Int {
    val grid = buildGrid(input)
    val start = Beam(Coord(0, 0), east)

    val alreadySeen = calculatePropagation(grid, start)
    val alreadyEnergized = calculateEnergization(alreadySeen)

    return alreadyEnergized.size
}

fun main() {
    val input = File("input.txt").readLines()
    println(solve(input))
}