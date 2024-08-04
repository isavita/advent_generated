import java.io.File

data class Coordinate(val x: Int, val y: Int, val z: Int)

val zero = Coordinate(0, 0, 0)

fun Coordinate.distance(a: Coordinate) = kotlin.math.abs(x - a.x) + kotlin.math.abs(y - a.y) + kotlin.math.abs(z - a.z)

typealias Bots = MutableMap<Coordinate, MutableList<Int>>

fun newBots(input: List<String>): Bots {
    val bots = mutableMapOf<Coordinate, MutableList<Int>>()
    for (data in input) {
        val (x, y, z, r) = data.replace("pos=<", "").replace(">, r=", ",").split(",").map { it.toInt() }
        val coord = Coordinate(x, y, z)
        bots.computeIfAbsent(coord) { mutableListOf() }.add(r)
    }
    return bots
}

fun Bots.haveInRange(pos: Coordinate): Int {
    var sum = 0
    for ((c, rs) in this) {
        for (r in rs) {
            if (pos.distance(c) <= r) sum++
        }
    }
    return sum
}

fun strongestReachable(bots: Bots): Int {
    var largestRadius = 0
    var largestPos = zero
    for ((c, rs) in bots) {
        for (r in rs) {
            if (r > largestRadius) {
                largestPos = c
                largestRadius = r
            }
        }
    }
    var count = 0
    for ((c, rs) in bots) {
        if (largestPos.distance(c) <= largestRadius) count += rs.size
    }
    return count
}

fun closestSuccess(bots: Bots): Int {
    var cur = zero
    var topLeft = zero
    var bottomRight = zero
    var zoom = 1 shl (32 - 2)

    while (true) {
        val zoomedBots = mutableMapOf<Coordinate, MutableList<Int>>()
        val best = mutableMapOf<Coordinate, Int>()
        for ((c, rs) in bots) {
            for (r in rs) {
                val zc = Coordinate(c.x / zoom, c.y / zoom, c.z / zoom)
                zoomedBots.computeIfAbsent(zc) { mutableListOf() }.add(r / zoom)
            }
        }

        for (x in topLeft.x..bottomRight.x) {
            for (y in topLeft.y..bottomRight.y) {
                for (z in topLeft.z..bottomRight.z) {
                    val pos = Coordinate(x, y, z)
                    val c = zoomedBots.haveInRange(pos)
                    if (best.isEmpty() || c > best.values.first()) {
                        best.clear()
                        best[pos] = c
                    } else if (c == best.values.first() && zero.distance(pos) < zero.distance(best.keys.first())) {
                        best.clear()
                        best[pos] = c
                    }
                }
            }
        }

        if (best.isEmpty()) {
            // If no position is in range of any bot, return the distance to the origin
            return zero.distance(topLeft)
        }

        if (zoom == 1) {
            return zero.distance(best.keys.first())
        }

        topLeft = Coordinate((best.keys.first().x - 1) shl 1, (best.keys.first().y - 1) shl 1, (best.keys.first().z - 1) shl 1)
        bottomRight = Coordinate((best.keys.first().x + 1) shl 1, (best.keys.first().y + 1) shl 1, (best.keys.first().z + 1) shl 1)
        zoom = zoom shr 1
    }
}

fun main() {
    val input = File("input.txt").readLines()
    val bots = newBots(input)
    println(closestSuccess(bots))
}