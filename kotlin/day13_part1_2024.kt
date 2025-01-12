
import java.io.File
import kotlin.math.min

data class Machine(var ax: Int, var ay: Int, var bx: Int, var by: Int, var px: Int, var py: Int)

fun main() {
    val machines = readInput("input.txt")
    val results = machines.mapNotNull { solveMachine(it) }

    if (results.isEmpty()) {
        println("0 0")
        return
    }

    val count = results.size
    val sum = results.sum()
    println("$count $sum")
}

fun readInput(filename: String): List<Machine> {
    val machines = mutableListOf<Machine>()
    val lines = mutableListOf<String>()

    File(filename).forEachLine { line ->
        val trimmedLine = line.trim()
        if (trimmedLine.isEmpty()) {
            if (lines.isNotEmpty()) {
                machines.add(parseMachine(lines))
                lines.clear()
            }
        } else {
            lines.add(trimmedLine)
        }
    }
    if (lines.isNotEmpty()) {
        machines.add(parseMachine(lines))
    }
    return machines
}

fun parseMachine(lines: List<String>): Machine {
    var ax = 0
    var ay = 0
    var bx = 0
    var by = 0
    var px = 0
    var py = 0

    for (line in lines) {
        when {
            line.startsWith("Button A:") -> {
                val (x, y) = parseLine(line.substringAfter("Button A:").trim())
                ax = x
                ay = y
            }
            line.startsWith("Button B:") -> {
                val (x, y) = parseLine(line.substringAfter("Button B:").trim())
                bx = x
                by = y
            }
            line.startsWith("Prize:") -> {
                val (x, y) = parsePrize(line.substringAfter("Prize:").trim())
                px = x
                py = y
            }
        }
    }
    return Machine(ax, ay, bx, by, px, py)
}

fun parseLine(s: String): Pair<Int, Int> {
    val parts = s.split(",").map { it.trim() }
    val x = parseVal(parts[0])
    val y = parseVal(parts[1])
    return Pair(x, y)
}

fun parsePrize(s: String): Pair<Int, Int> {
    val parts = s.split(",").map { it.trim() }
    val x = parseValPrize(parts[0])
    val y = parseValPrize(parts[1])
    return Pair(x, y)
}

fun parseVal(s: String): Int {
    val trimmed = s.trim().removePrefix("X+").removePrefix("Y+").removePrefix("X=").removePrefix("Y=")
    return trimmed.toInt()
}

fun parseValPrize(s: String): Int {
    val trimmed = s.trim().removePrefix("X=").removePrefix("Y=")
    return trimmed.toInt()
}

fun solveMachine(m: Machine): Int? {
    var minCost: Int? = null
    for (aCount in 0..100) {
        for (bCount in 0..100) {
            val x = m.ax * aCount + m.bx * bCount
            val y = m.ay * aCount + m.by * bCount
            if (x == m.px && y == m.py) {
                val cost = aCount * 3 + bCount
                minCost = minCost?.let { min(it, cost) } ?: cost
            }
        }
    }
    return minCost
}
