
import java.io.File

data class Machine(var ax: Long, var ay: Long, var bx: Long, var by: Long, var px: Long, var py: Long)

fun main() {
    val offset = 10000000000000L
    val machines = readInput("input.txt")
    machines.forEach {
        it.px += offset
        it.py += offset
    }

    val results = machines.mapNotNull { solveMachine(it) }.filter { it >= 0 }
    if (results.isEmpty()) {
        println("0 0")
    } else {
        println("${results.size} ${results.sum()}")
    }
}

fun readInput(filename: String): List<Machine> {
    val lines = File(filename).readLines()
    val machines = mutableListOf<Machine>()
    var currentMachineLines = mutableListOf<String>()
    for (line in lines) {
        val trimmedLine = line.trim()
        if (trimmedLine.isEmpty()) {
            if (currentMachineLines.isNotEmpty()) {
                machines.add(parseMachine(currentMachineLines))
                currentMachineLines.clear()
            }
        } else {
            currentMachineLines.add(trimmedLine)
        }
    }
    if (currentMachineLines.isNotEmpty()) {
        machines.add(parseMachine(currentMachineLines))
    }
    return machines
}

fun parseMachine(lines: List<String>): Machine {
    val m = Machine(0, 0, 0, 0, 0, 0)
    for (l in lines) {
        val line = l.replace("Button A:", "A:").replace("Button B:", "B:").replace("Prize:", "P:")
        when {
            line.startsWith("A:") -> {
                val (x, y) = parseLine(line.substring(2))
                m.ax = x
                m.ay = y
            }
            line.startsWith("B:") -> {
                val (x, y) = parseLine(line.substring(2))
                m.bx = x
                m.by = y
            }
            line.startsWith("P:") -> {
                val (x, y) = parsePrize(line.substring(2))
                m.px = x
                m.py = y
            }
        }
    }
    return m
}

fun parseLine(s: String): Pair<Long, Long> {
    val parts = s.trim().split(",")
    return Pair(parseVal(parts[0]), parseVal(parts[1]))
}

fun parsePrize(s: String): Pair<Long, Long> {
    val parts = s.trim().split(",")
    return Pair(parseValPrize(parts[0]), parseValPrize(parts[1]))
}

fun parseVal(s: String): Long {
    return s.trim().removePrefix("X+").removePrefix("Y+").removePrefix("X=").removePrefix("Y=").toLong()
}

fun parseValPrize(s: String): Long {
    return s.trim().removePrefix("X=").removePrefix("Y=").toLong()
}

fun solveMachine(m: Machine): Long? {
    val d = m.ax * m.by - m.ay * m.bx
    if (d == 0L) return null
    val numA = m.px * m.by - m.py * m.bx
    val numB = -m.px * m.ay + m.py * m.ax
    if (numA % d != 0L || numB % d != 0L) return null
    val a = numA / d
    val b = numB / d
    if (a < 0 || b < 0) return null
    return 3 * a + b
}
