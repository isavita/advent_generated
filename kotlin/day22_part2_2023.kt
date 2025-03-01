
import java.io.File

data class Brick(val id: Int, val x1: Int, val y1: Int, val z1: Int, val x2: Int, val y2: Int, val z2: Int) {
    fun intersects(other: Brick): Boolean {
        return maxOf(x1, other.x1) <= minOf(x2, other.x2) &&
               maxOf(y1, other.y1) <= minOf(y2, other.y2)
    }

    fun dropTo(z: Int): Brick {
        val diff = z1 - z
        return copy(z1 = z1 - diff, z2 = z2 - diff)
    }
    
    fun getBaseCoordinates(): List<Pair<Int, Int>> {
        val coords = mutableListOf<Pair<Int, Int>>()
        for (x in x1..x2) {
            for (y in y1..y2) {
                coords.add(Pair(x, y))
            }
        }
        return coords
    }
}

fun main() {
    val input = File("input.txt").readLines()
    val bricks = parseBricks(input)
    val settledBricks = settleBricks(bricks)

    val (safeToDisintegrate, supports) = analyzeSupports(settledBricks)
    println("Part 1: ${safeToDisintegrate.size}")

    val totalFallenBricks = (0 until settledBricks.size).sumOf { i ->
        countFallenBricks(i, settledBricks, supports)
    }
    println("Part 2: $totalFallenBricks")
}

fun parseBricks(input: List<String>): List<Brick> {
    return input.mapIndexed { index, line ->
        val (start, end) = line.split("~")
        val (x1, y1, z1) = start.split(",").map { it.toInt() }
        val (x2, y2, z2) = end.split(",").map { it.toInt() }
        Brick(index, x1, y1, z1, x2, y2, z2)
    }
}

fun settleBricks(bricks: List<Brick>): List<Brick> {
    val sortedBricks = bricks.sortedBy { it.z1 }
    val settledBricks = mutableListOf<Brick>()

    for (brick in sortedBricks) {
        var maxZ = 1
        for (settled in settledBricks) {
            if (brick.intersects(settled)) {
                maxZ = maxOf(maxZ, settled.z2 + 1)
            }
        }
        settledBricks.add(brick.dropTo(maxZ))
    }
    return settledBricks.sortedBy { it.z1 }
}

fun analyzeSupports(bricks: List<Brick>): Pair<Set<Int>, Map<Int, Set<Int>>> {
    val supports = mutableMapOf<Int, MutableSet<Int>>()
    val supportedBy = mutableMapOf<Int, MutableSet<Int>>()

    for (upper in bricks) {
        for (lower in bricks) {
            if (upper.intersects(lower) && upper.z1 == lower.z2 + 1) {
                supports.getOrPut(lower.id) { mutableSetOf() }.add(upper.id)
                supportedBy.getOrPut(upper.id) { mutableSetOf() }.add(lower.id)
            }
        }
    }
    
    val safeToDisintegrate = bricks.map { it.id }.toMutableSet()
    for ((_, supported) in supports) {
      if(supported.size == 1){
            safeToDisintegrate.remove(supportedBy[supported.first()]!!.first())
      }
    }

    return Pair(safeToDisintegrate, supports)
}

fun countFallenBricks(brickId: Int, bricks: List<Brick>, supports: Map<Int, Set<Int>>): Int {
    val fallen = mutableSetOf(brickId)
    val queue = mutableListOf(brickId)

    while (queue.isNotEmpty()) {
        val currentId = queue.removeFirst()
        val supportedBricks = supports[currentId] ?: emptySet()
         for (supportedId in supportedBricks) {
                val supportedBySetForSupportedBrick = bricks.firstOrNull {it.id == supportedId}
                        ?.let { brick ->
                            bricks.filter { other ->
                                brick.intersects(other) && brick.z1 == other.z2 + 1
                            }.map {it.id}.toSet()
                        }?: emptySet()


                if (supportedBySetForSupportedBrick.all { it in fallen }) {
                    if (fallen.add(supportedId)) {
                        queue.add(supportedId)
                    }
                }
         }
    }
    return fallen.size - 1 // Subtract the initial disintegrated brick
}
