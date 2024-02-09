import java.io.File

data class Coord(val x: Int, val y: Int) {
    fun add(c: Coord) = Coord(x + c.x, y + c.y)
}

data class Beam(val origin: Coord, val dir: Coord)

data class Grid(val width: Int, val height: Int, val data: MutableMap<Coord, Char>)

val Empty = '.'
val AscendingMirror = '/'
val DescendingMirror = '\\'
val VerticalSplitter = '|'
val HorizontalSplitter = '-'

val North = Coord(0, -1)
val West = Coord(-1, 0)
val South = Coord(0, 1)
val East = Coord(1, 0)

fun Coord.rotate90() = Coord(y, -x)
fun Coord.rotateNeg90() = Coord(-y, x)
fun Coord.isInBounds(grid: Grid) = x in 0 until grid.width && y in 0 until grid.height

fun buildGrid(input: List<String>): Grid {
    val grid = Grid(input[0].length, input.size, mutableMapOf())

    input.forEachIndexed { y, line ->
        line.forEachIndexed { x, char ->
            if (char != Empty) {
                grid.data[Coord(x, y)] = char
            }
        }
    }

    return grid
}

fun Grid.toString(): String {
    var result = ""

    for (y in 0 until height) {
        for (x in 0 until width) {
            val coord = Coord(x, y)
            result += data[coord] ?: Empty
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
        char == AscendingMirror -> {
            val newDir = if (beam.dir == North || beam.dir == South) beam.dir.rotateNeg90() else beam.dir.rotate90()
            val newBeam = Beam(beam.origin.add(newDir), newDir)
            beams.add(newBeam)
        }
        char == DescendingMirror -> {
            val newDir = if (beam.dir == North || beam.dir == South) beam.dir.rotate90() else beam.dir.rotateNeg90()
            val newBeam = Beam(beam.origin.add(newDir), newDir)
            beams.add(newBeam)
        }
        char == VerticalSplitter && (beam.dir == East || beam.dir == West) -> {
            val newDir1 = beam.dir.rotate90()
            val newBeam1 = Beam(beam.origin.add(newDir1), newDir1)
            val newDir2 = beam.dir.rotateNeg90()
            val newBeam2 = Beam(beam.origin.add(newDir2), newDir2)
            beams.addAll(listOf(newBeam1, newBeam2))
        }
        char == HorizontalSplitter && (beam.dir == North || beam.dir == South) -> {
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

fun calculatePropagation(grid: Grid, start: Beam): Map<Beam, Unit> {
    val alreadySeen = mutableMapOf<Beam, Unit>()
    val toExplore = mutableListOf(start)

    while (toExplore.isNotEmpty()) {
        val beam = toExplore.removeAt(0)
        if (beam.origin.isInBounds(grid) && beam !in alreadySeen) {
            alreadySeen[beam] = Unit
            toExplore.addAll(nextBeam(grid, beam))
        }
    }

    return alreadySeen
}

fun buildBeamGrid(grid: Grid, alreadySeen: Map<Beam, Unit>): Grid {
    val beamGrid = Grid(grid.width, grid.height, mutableMapOf())
    beamGrid.data.putAll(grid.data)

    alreadySeen.keys.forEach { beam ->
        if (beam.origin !in grid.data) {
            if (beam.origin in beamGrid.data) {
                beamGrid.data[beam.origin] = '2'
            } else {
                beamGrid.data[beam.origin] = when (beam.dir) {
                    North -> '^'
                    East -> '>'
                    South -> 'v'
                    West -> '<'
                    else -> throw IllegalArgumentException("Invalid direction")
                }
            }
        }
    }

    return beamGrid
}

fun calculateEnergization(alreadySeen: Map<Beam, Unit>): Map<Coord, Unit> {
    val alreadyEnergized = mutableMapOf<Coord, Unit>()
    alreadySeen.keys.forEach { beam ->
        alreadyEnergized[beam.origin] = Unit
    }
    return alreadyEnergized
}

fun getBorder(grid: Grid): List<Beam> {
    val border = mutableListOf<Beam>()

    for (x in 0 until grid.width) {
        val coord = Coord(x, 0)
        border.add(Beam(coord, South))

        val coord2 = Coord(x, grid.height - 1)
        border.add(Beam(coord2, North))
    }

    for (y in 0 until grid.height) {
        val coord = Coord(0, y)
        border.add(Beam(coord, East))

        val coord2 = Coord(grid.width - 1, y)
        border.add(Beam(coord2, West))
    }

    return border
}

fun solve(input: List<String>): Int {
    val grid = buildGrid(input)
    val starts = getBorder(grid)

    var res = 0
    starts.forEach { start ->
        val alreadySeen = calculatePropagation(grid, start)
        val alreadyEnergized = calculateEnergization(alreadySeen)

        val energy = alreadyEnergized.size
        if (energy > res) {
            res = energy
        }
    }

    return res
}

fun main(args: Array<String>) {
    val input = File("input.txt").readLines()
    println(solve(input))
}