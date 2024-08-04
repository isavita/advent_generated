import kotlin.math.*
import java.io.File

data class Asteroid(val x: Int, val y: Int, var angle: Double, var dist: Double)

fun main() {
    val asteroids = readAsteroids("input.txt")
    val station = findBestAsteroidLocation(asteroids)
    val vaporized = vaporizeAsteroids(asteroids, station)
    if (vaporized.size >= 200) {
        val result = vaporized[199].x * 100 + vaporized[199].y
        println(result)
    } else {
        println("Less than 200 asteroids were vaporized.")
    }
}

fun readAsteroids(filename: String): List<List<Boolean>> {
    return File(filename).readLines().map { line -> line.map { it == '#' } }
}

fun vaporizeAsteroids(asteroids: List<List<Boolean>>, station: Pair<Int, Int>): List<Asteroid> {
    val targets = mutableListOf<Asteroid>()
    for (y in asteroids.indices) {
        for (x in asteroids[y].indices) {
            if (asteroids[y][x] && !(x == station.first && y == station.second)) {
                val angle = atan2(y - station.second.toDouble(), x - station.first.toDouble())
                val dist = hypot(x - station.first.toDouble(), y - station.second.toDouble())
                targets.add(Asteroid(x, y, if (angle < -PI / 2) angle + 2 * PI else angle, dist))
            }
        }
    }

    targets.sortWith(compareBy({ it.angle }, { it.dist }))

    val vaporized = mutableListOf<Asteroid>()
    while (targets.isNotEmpty()) {
        val angles = targets.map { it.angle }.toSet()
        for (angle in angles) {
            val asteroid = targets.find { it.angle == angle }
            if (asteroid != null) {
                vaporized.add(asteroid)
                targets.remove(asteroid)
            }
        }
    }
    return vaporized
}

fun findBestAsteroidLocation(asteroids: List<List<Boolean>>): Pair<Int, Int> {
    var bestLocation = Pair(0, 0)
    var maxCount = 0
    for (y in asteroids.indices) {
        for (x in asteroids[y].indices) {
            if (asteroids[y][x]) {
                val count = countVisibleAsteroids(asteroids, x, y)
                if (count > maxCount) {
                    maxCount = count
                    bestLocation = Pair(x, y)
                }
            }
        }
    }
    return bestLocation
}

fun countVisibleAsteroids(asteroids: List<List<Boolean>>, x: Int, y: Int): Int {
    val angles = mutableSetOf<Double>()
    for (otherY in asteroids.indices) {
        for (otherX in asteroids[otherY].indices) {
            if (asteroids[otherY][otherX] && !(otherX == x && otherY == y)) {
                angles.add(atan2(otherY - y.toDouble(), otherX - x.toDouble()))
            }
        }
    }
    return angles.size
}