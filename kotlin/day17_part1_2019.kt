
import java.io.File
import java.util.*
import kotlin.math.abs

enum class Mode {
    POSITION, IMMEDIATE, RELATIVE
}

enum class Opcode(val code: Int) {
    ADD(1), MUL(2), INPUT(3), OUTPUT(4), JT(5), JF(6), LT(7), EQ(8), RBO(9), HALT(99)
}

data class DecodedInstruction(val opcode: Opcode, val modes: List<Mode>)

fun decode(n: Int): DecodedInstruction {
    val op = Opcode.values().firstOrNull { it.code == n % 100 } ?: throw IllegalArgumentException("Unknown opcode: ${n % 100}")
    var num = n / 100
    val modes = mutableListOf<Mode>()
    for (i in 0 until 3) {
        modes.add(
            when (num % 10) {
                0 -> Mode.POSITION
                1 -> Mode.IMMEDIATE
                2 -> Mode.RELATIVE
                else -> throw IllegalArgumentException("Unknown mode: ${num % 10}")
            }
        )
        num /= 10
    }
    return DecodedInstruction(op, modes)
}

class Machine(
    program: List<Int>,
    private val input: Queue<Int>,
    private val output: MutableList<Int>
) {
    private val data: MutableMap<Int, Int> = program.indices.associate { it to program[it] }.toMutableMap()
    private var ip = 0
    private var relbase = 0

    private fun get(i: Int, mode: Mode): Int {
        return when (mode) {
            Mode.IMMEDIATE -> data[i] ?: 0
            Mode.POSITION -> data[data[i] ?: 0] ?: 0
            Mode.RELATIVE -> data[relbase + (data[i] ?: 0)] ?: 0
        }
    }

    private fun set(i: Int, mode: Mode, value: Int) {
        when (mode) {
            Mode.POSITION -> data[data[i] ?: 0] = value
            Mode.RELATIVE -> data[relbase + (data[i] ?: 0)] = value
            else -> throw IllegalArgumentException("Invalid mode for set operation: $mode")
        }
    }

    fun step(): Boolean {
        val (op, modes) = decode(data[ip] ?: 0)
        when (op) {
            Opcode.ADD -> {
                val value = get(ip + 1, modes[0]) + get(ip + 2, modes[1])
                set(ip + 3, modes[2], value)
                ip += 4
            }
            Opcode.MUL -> {
                val value = get(ip + 1, modes[0]) * get(ip + 2, modes[1])
                set(ip + 3, modes[2], value)
                ip += 4
            }
            Opcode.INPUT -> {
                set(ip + 1, modes[0], input.poll() ?: throw IllegalStateException("Input queue is empty"))
                ip += 2
            }
            Opcode.OUTPUT -> {
                output.add(get(ip + 1, modes[0]))
                ip += 2
            }
            Opcode.JT -> {
                if (get(ip + 1, modes[0]) != 0) {
                    ip = get(ip + 2, modes[1])
                } else {
                    ip += 3
                }
            }
            Opcode.JF -> {
                if (get(ip + 1, modes[0]) == 0) {
                    ip = get(ip + 2, modes[1])
                } else {
                    ip += 3
                }
            }
            Opcode.LT -> {
                val value = if (get(ip + 1, modes[0]) < get(ip + 2, modes[1])) 1 else 0
                set(ip + 3, modes[2], value)
                ip += 4
            }
            Opcode.EQ -> {
                val value = if (get(ip + 1, modes[0]) == get(ip + 2, modes[1])) 1 else 0
                set(ip + 3, modes[2], value)
                ip += 4
            }
            Opcode.RBO -> {
                relbase += get(ip + 1, modes[0])
                ip += 2
            }
            Opcode.HALT -> return false
        }
        return true
    }

    fun run() {
        while (step()) {
        }
    }
}

fun runProgram(program: List<Int>, input: Queue<Int>): List<Int> {
    val output = mutableListOf<Int>()
    val machine = Machine(program, input, output)
    machine.run()
    return output
}

enum class Dir {
    N, E, S, W
}

data class Point(val x: Int, val y: Int) {
    operator fun plus(other: Point): Point = Point(x + other.x, y + other.y)
}

val pointMap = mapOf(Dir.N to Point(0, 1), Dir.E to Point(1, 0), Dir.S to Point(0, -1), Dir.W to Point(-1, 0))
val pointReversedMap = mapOf(Dir.N to Point(0, -1), Dir.E to Point(1, 0), Dir.S to Point(0, 1), Dir.W to Point(-1, 0))

fun Dir.toPoint(): Point = pointMap[this]!!
fun Dir.toPointReversed(): Point = pointReversedMap[this]!!

val neighbors4 = listOf(Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0))

fun abs(n: Int): Int = if (n < 0) -n else n

val fromPointMap = mapOf(Point(0, 1) to Dir.N, Point(1, 0) to Dir.E, Point(0, -1) to Dir.S, Point(-1, 0) to Dir.W)
fun pointToDir(p: Point): Dir = fromPointMap[p]!!

fun Dir.next(): Dir = Dir.values()[(ordinal + 1) % 4]
fun Dir.prev(): Dir = Dir.values()[(ordinal + 3) % 4]

val fromByteMap = mapOf(
    'N' to Dir.N, 'E' to Dir.E, 'S' to Dir.S, 'W' to Dir.W,
    'U' to Dir.N, 'R' to Dir.E, 'D' to Dir.S, 'L' to Dir.W,
    '^' to Dir.N, '>' to Dir.E, 'v' to Dir.S, '<' to Dir.W
)

fun byteToDir(b: Char): Dir = fromByteMap[b]!!

fun readAll(filepath: String): String = File(filepath).readText().trim()

fun String.toIntList(): List<Int> = this.split(",").map { it.toInt() }

fun main() {
    val program = readAll("input.txt").toIntList()
    val (scaffolding, _, _) = parse(program)
    println(sumAlign(scaffolding))
}

fun parse(program: List<Int>): Triple<Set<Point>, Point, Dir> {
    val output = runProgram(program, LinkedList())
    val sb = StringBuilder()
    output.forEach { sb.append(it.toChar()) }

    val scaffolding = mutableSetOf<Point>()
    var robot = Point(0,0)
    var dir = Dir.N
    var y = 0
    sb.toString().lines().forEach { line ->
        for (x in line.indices) {
            when (line[x]) {
                '^', 'v', '<', '>' -> {
                    robot = Point(x, y)
                    dir = byteToDir(line[x])
                    scaffolding.add(Point(x, y))
                }
                '#' -> scaffolding.add(Point(x, y))
            }
        }
        y++
    }
    return Triple(scaffolding, robot, dir)
}

fun sumAlign(grid: Set<Point>): Int {
    var sum = 0
    for (p in grid) {
        if (neighbors4.all { grid.contains(p + it) }) {
            sum += p.x * p.y
        }
    }
    return sum
}

fun path(scaffolding: Set<Point>, robot: Point, dir: Dir): String {
    var currentRobot = robot
    var currentDir = dir
    var dist = 0
    var d: Char = ' '
    val sections = mutableListOf<String>()

    while (true) {
        if (scaffolding.contains(currentRobot + currentDir.toPointReversed())) {
            currentRobot += currentDir.toPointReversed()
            dist++
            continue
        }
        if (dist > 0) {
            sections.add("$d,$dist")
        }
        if (scaffolding.contains(currentRobot + currentDir.next().toPointReversed())) {
            currentRobot += currentDir.next().toPointReversed()
            currentDir = currentDir.next()
            dist = 1
            d = 'R'
        } else if (scaffolding.contains(currentRobot + currentDir.prev().toPointReversed())) {
            currentRobot += currentDir.prev().toPointReversed()
            currentDir = currentDir.prev()
            dist = 1
            d = 'L'
        } else {
            break
        }
    }
    return sections.joinToString(",")
}

fun encode(path: String): Triple<String, String, String> {
    var seq = ""
    var a = ""
    var b = ""
    var c = ""
    loop@ for (i in 2..21) {
        for (j in 2..21) {
            for (k in 2..21) {
                var next = "$path,"
                a = next.substring(0, i)
                next = next.replace(a, "")
                b = next.substring(0, j)
                next = next.replace(b, "")
                c = next.substring(0, k)
                next = next.replace(c, "")
                if (next == "") {
                    break@loop
                }
            }
        }
    }
    a = a.trim(',')
    b = b.trim(',')
    c = c.trim(',')
    var currentPath = path
    currentPath = currentPath.replace(a, "A")
    currentPath = currentPath.replace(b, "B")
    currentPath = currentPath.replace(c, "C")
    seq = currentPath.trim(',')
    return Triple(seq, a, b)
}
