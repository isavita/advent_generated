import java.io.File

data class Nanobot(val x: Int, val y: Int, val z: Int, val radius: Int)

fun main(args: Array<String>) {
    val nanobots = parseNanobots(File("input.txt"))

    val strongest = findStrongestNanobot(nanobots)
    val inRangeCount = countNanobotsInRange(nanobots, strongest)

    println(inRangeCount)
}

fun parseNanobots(file: File): List<Nanobot> {
    val nanobots = mutableListOf<Nanobot>()
    val re = Regex("""pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)""")

    file.forEachLine {
        val matches = re.find(it)!!.destructured
        val (x, y, z, radius) = matches.toList()

        nanobots.add(Nanobot(x.toInt(), y.toInt(), z.toInt(), radius.toInt()))
    }

    return nanobots
}

fun findStrongestNanobot(nanobots: List<Nanobot>): Nanobot {
    var strongest = nanobots.maxByOrNull { it.radius }!!
    return strongest
}

fun countNanobotsInRange(nanobots: List<Nanobot>, strongest: Nanobot): Int {
    var count = 0
    for (nanobot in nanobots) {
        if (manhattanDistance(nanobot, strongest) <= strongest.radius) {
            count++
        }
    }
    return count
}

fun manhattanDistance(a: Nanobot, b: Nanobot): Int {
    return abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z)
}

fun abs(x: Int): Int {
    return if (x < 0) -x else x
}