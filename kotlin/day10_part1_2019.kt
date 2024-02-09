import java.io.File
import kotlin.math.atan2

fun main(args: Array<String>) {
    val asteroids = readAsteroids("input.txt")
    val maxCount = findBestAsteroidLocation(asteroids)
    println(maxCount)
}

fun readAsteroids(filename: String): List<List<Boolean>> {
    val asteroids = mutableListOf<List<Boolean>>()
    File(filename).forEachLine {
        val asteroidRow = it.map { char -> char == '#' }
        asteroids.add(asteroidRow)
    }
    return asteroids
}

fun findBestAsteroidLocation(asteroids: List<List<Boolean>>): Int {
    var maxCount = 0
    for (y in asteroids.indices) {
        for (x in asteroids[y].indices) {
            if (asteroids[y][x]) {
                val count = countVisibleAsteroids(asteroids, x, y)
                if (count > maxCount) {
                    maxCount = count
                }
            }
        }
    }
    return maxCount
}

fun countVisibleAsteroids(asteroids: List<List<Boolean>>, x: Int, y: Int): Int {
    val angles = mutableSetOf<Double>()
    for (otherY in asteroids.indices) {
        for (otherX in asteroids[otherY].indices) {
            if (asteroids[otherY][otherX] && !(otherX == x && otherY == y)) {
                val angle = atan2((otherY - y).toDouble(), (otherX - x).toDouble())
                angles.add(angle)
            }
        }
    }
    return angles.size
}