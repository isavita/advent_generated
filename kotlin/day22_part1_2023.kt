
import java.io.File
import kotlin.math.max
import kotlin.math.min

data class Coord(var x: Int, var y: Int, var z: Int)
data class Brick(var mini: Coord, var maxi: Coord, val basedOn: MutableList<Brick> = mutableListOf(), val support: MutableList<Brick> = mutableListOf())

fun parseInput(input: List<String>): List<Brick> {
    return input.map { line ->
        val (start, end) = line.split("~")
        val (x1, y1, z1) = start.split(",").map { it.toInt() }
        val (x2, y2, z2) = end.split(",").map { it.toInt() }
        Brick(Coord(x1, y1, z1), Coord(x2, y2, z2))
    }
}

fun settle(bricks: MutableList<Brick>) {
    bricks.sortBy { it.maxi.z }

    for (i in bricks.indices) {
        val brick = bricks[i]
        var supportZ = 0
        val basedBricks = mutableListOf<Brick>()

        for (j in i - 1 downTo 0) {
            val other = bricks[j]
            val isIntersectingX = max(brick.mini.x, other.mini.x) <= min(brick.maxi.x, other.maxi.x)
            val isIntersectingY = max(brick.mini.y, other.mini.y) <= min(brick.maxi.y, other.maxi.y)
            if (isIntersectingX && isIntersectingY) {
                when {
                    other.maxi.z == supportZ -> basedBricks.add(other)
                    other.maxi.z > supportZ -> {
                        supportZ = other.maxi.z
                        basedBricks.clear()
                        basedBricks.add(other)
                    }
                }
            }
        }

        brick.basedOn.addAll(basedBricks)
        basedBricks.forEach { it.support.add(brick) }

        val deltaZ = brick.maxi.z - brick.mini.z
        brick.mini.z = supportZ + 1
        brick.maxi.z = brick.mini.z + deltaZ
    }
}

fun solve(input: List<String>): Int {
    val bricks = parseInput(input).toMutableList()
    settle(bricks)

    return bricks.count { brick ->
        brick.support.all { it.basedOn.size > 1 }
    }
}

fun main() {
    val input = File("input.txt").readLines()
    println(solve(input))
}
