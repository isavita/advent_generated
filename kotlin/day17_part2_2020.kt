import java.io.File

data class Coordinate4D(val x: Int, val y: Int, val z: Int, val w: Int)

fun main(args: Array<String>) {
    val initialState = File("input.txt").readLines()
    var activeCubes = mutableMapOf<Coordinate4D, Boolean>()

    initialState.forEachIndexed { y, line ->
        line.forEachIndexed { x, char ->
            if (char == '#') {
                activeCubes[Coordinate4D(x, y, 0, 0)] = true
            }
        }
    }

    repeat(6) {
        activeCubes = simulateCycle4D(activeCubes)
    }

    println(activeCubes.size)
}

fun simulateCycle4D(activeCubes: Map<Coordinate4D, Boolean>): MutableMap<Coordinate4D, Boolean> {
    val newActiveCubes = mutableMapOf<Coordinate4D, Boolean>()
    val neighborCounts = mutableMapOf<Coordinate4D, Int>()

    activeCubes.keys.forEach { coord ->
        for (dw in -1..1) {
            for (dz in -1..1) {
                for (dy in -1..1) {
                    for (dx in -1..1) {
                        if (dw == 0 && dz == 0 && dy == 0 && dx == 0) {
                            continue
                        }
                        val neighbor = Coordinate4D(coord.x + dx, coord.y + dy, coord.z + dz, coord.w + dw)
                        neighborCounts[neighbor] = neighborCounts.getOrDefault(neighbor, 0) + 1
                    }
                }
            }
        }
    }

    neighborCounts.forEach { (coord, count) ->
        if (count == 3 || (count == 2 && activeCubes[coord] == true)) {
            newActiveCubes[coord] = true
        }
    }

    return newActiveCubes
}