import java.io.File
import kotlin.collections.HashMap

data class Coordinate(val x: Int, val y: Int, val z: Int)

fun main(args: Array<String>) {
    val initialState = File("input.txt").readLines()
    val activeCubes = HashMap<Coordinate, Boolean>()

    initialState.forEachIndexed { y, line ->
        line.forEachIndexed { x, char ->
            if (char == '#') {
                activeCubes[Coordinate(x, y, 0)] = true
            }
        }
    }

    repeat(6) {
        activeCubes.simulateCycle()
    }

    println(activeCubes.size)
}

fun HashMap<Coordinate, Boolean>.simulateCycle() {
    val newActiveCubes = HashMap<Coordinate, Boolean>()
    val neighborCounts = HashMap<Coordinate, Int>()

    this.keys.forEach { coord ->
        (-1..1).forEach { dz ->
            (-1..1).forEach { dy ->
                (-1..1).forEach { dx ->
                    if (dz == 0 && dy == 0 && dx == 0) {
                        return@forEach
                    }
                    val neighbor = Coordinate(coord.x + dx, coord.y + dy, coord.z + dz)
                    neighborCounts[neighbor] = neighborCounts.getOrDefault(neighbor, 0) + 1
                }
            }
        }
    }

    neighborCounts.forEach { (coord, count) ->
        if (count == 3 || (count == 2 && this[coord] == true)) {
            newActiveCubes[coord] = true
        }
    }

    this.clear()
    this.putAll(newActiveCubes)
}