
import java.io.File
import java.util.regex.Pattern

data class Robot(var x: Int, var y: Int, val vx: Int, val vy: Int)

fun mod(a: Int, b: Int): Int = (a % b + b) % b

fun parseLine(line: String): Robot {
    val pattern = Pattern.compile("""p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)""")
    val matcher = pattern.matcher(line)
    if (!matcher.find()) {
        throw IllegalArgumentException("Invalid line format: $line")
    }
    val x = matcher.group(1).toInt()
    val y = matcher.group(2).toInt()
    val vx = matcher.group(3).toInt()
    val vy = matcher.group(4).toInt()
    return Robot(x, y, vx, vy)
}

fun moveRobots(robots: List<Robot>, sizeX: Int, sizeY: Int) {
    for (robot in robots) {
        robot.x = mod(robot.x + robot.vx, sizeX)
        robot.y = mod(robot.y + robot.vy, sizeY)
    }
}

fun countQuadrants(robots: List<Robot>, sizeX: Int, sizeY: Int): IntArray {
    val counts = IntArray(4)
    val centerX = sizeX / 2
    val centerY = sizeY / 2

    for (robot in robots) {
        val x = robot.x
        val y = robot.y
        when {
            x < centerX && y < centerY -> counts[0]++
            x < centerX && y > centerY -> counts[1]++
            x > centerX && y < centerY -> counts[2]++
            x > centerX && y > centerY -> counts[3]++
        }
    }
    return counts
}

fun hasNoOverlaps(robots: List<Robot>): Boolean {
    val positions = HashSet<Pair<Int, Int>>()
    for (robot in robots) {
        val pos = Pair(robot.x, robot.y)
        if (!positions.add(pos)) {
            return false
        }
    }
    return true
}

fun drawGrid(robots: List<Robot>, sizeX: Int, sizeY: Int) {
    val gridMap = robots.associate { Pair(it.x, it.y) to true }
    for (y in 0 until sizeY) {
        val line = StringBuilder()
        for (x in 0 until sizeX) {
            line.append(if (gridMap[Pair(x, y)] == true) '#' else '.')
        }
        println(line.toString())
    }
}

fun main() {
    val sizeX = 101
    val sizeY = 103

    val robots = File("input.txt").readLines()
        .filter { it.isNotBlank() }
        .map { parseLine(it) }

    // Part 1
    val robotsPart1 = robots.map { it.copy() }
    repeat(100) {
        moveRobots(robotsPart1, sizeX, sizeY)
    }
    val counts = countQuadrants(robotsPart1, sizeX, sizeY)
    val safetyFactor = counts.reduce(Int::times)
    println("Part 1 - Safety Factor after 100 seconds: $safetyFactor")

    // Part 2
    val robotsPart2 = robots.map { it.copy() }
    var seconds = 0
    while (true) {
        if (hasNoOverlaps(robotsPart2)) {
            break
        }
        moveRobots(robotsPart2, sizeX, sizeY)
        seconds++
        if (seconds > 1000000) {
            println("Exceeded maximum iterations without finding a unique position configuration.")
            return
        }
    }
    println("Part 2 - Fewest seconds to display Easter egg: $seconds")
    println("Final positions of robots:")
    drawGrid(robotsPart2, sizeX, sizeY)
}
