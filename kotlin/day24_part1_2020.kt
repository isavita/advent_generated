import java.io.File

data class Coordinate(val q: Int, val r: Int)

val directions = mapOf(
    "e" to Coordinate(1, 0),
    "se" to Coordinate(0, 1),
    "sw" to Coordinate(-1, 1),
    "w" to Coordinate(-1, 0),
    "nw" to Coordinate(0, -1),
    "ne" to Coordinate(1, -1)
)

fun main(args: Array<String>) {
    val blackTiles = mutableMapOf<Coordinate, Boolean>()
    File("input.txt").forEachLine { line ->
        var i = 0
        var coord = Coordinate(0, 0)
        while (i < line.length) {
            val dir = when (line[i]) {
                'e', 'w' -> line[i].toString()
                else -> {
                    i++
                    line.substring(i - 1, i + 1)
                }
            }
            val move = directions[dir]!!
            coord = Coordinate(coord.q + move.q, coord.r + move.r)
            i++
        }
        blackTiles[coord] = blackTiles.getOrDefault(coord, false).not()
    }

    val count = blackTiles.count { it.value }
    println(count)
}