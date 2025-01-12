
import java.io.File

fun main() {
    val width = 101
    val height = 103
    val robots = File("input.txt").readLines().map { line ->
        val parts = line.split(" ")
        val pPart = parts[0].substringAfter("p=")
        val vPart = parts[1].substringAfter("v=")
        val pos = pPart.split(",").map { it.toInt() }
        val vel = vPart.split(",").map { it.toInt() }
        intArrayOf(pos[0], pos[1], vel[0], vel[1])
    }.toMutableList()

    repeat(100) {
        for (i in robots.indices) {
            val r = robots[i]
            var x = (r[0] + r[2]) % width
            var y = (r[1] + r[3]) % height
            if (x < 0) x += width
            if (y < 0) y += height
            robots[i][0] = x
            robots[i][1] = y
        }
    }

    var q1 = 0
    var q2 = 0
    var q3 = 0
    var q4 = 0
    for (r in robots) {
        val x = r[0]
        val y = r[1]
        if (x == 50 || y == 51) continue
        when {
            x < 50 && y < 51 -> q1++
            x > 50 && y < 51 -> q2++
            x < 50 && y > 51 -> q3++
            x > 50 && y > 51 -> q4++
        }
    }
    println(q1 * q2 * q3 * q4)
}
