
import java.io.File
import java.util.*

fun main() {
    val program = File("input.txt").readText().trim().split(",").map { it.toLong() }.toLongArray()
    val oxygenSystemLocation = findOxygenSystem(program)
    println(oxygenSystemLocation?.second ?: "Oxygen system not found")
}

fun findOxygenSystem(program: LongArray): Pair<Point, Int>? {
    val computer = IntcodeComputer(program)
    val visited = mutableMapOf<Point, Int>()
    val queue = LinkedList<Triple<Point, Int, IntcodeComputer>>()
    val start = Point(0, 0)
    queue.add(Triple(start, 0, computer))
    visited[start] = 0

    while (queue.isNotEmpty()) {
        val (current, steps, currentComputer) = queue.poll()

        for (direction in 1..4) {
            val nextComputer = currentComputer.copy()
            nextComputer.input.add(direction.toLong())
            nextComputer.run()
            val status = nextComputer.output.poll().toInt()
            val next = current.move(direction)

            if (status != 0 && visited[next] == null) {
                visited[next] = steps + 1
                if (status == 2) {
                    return Pair(next, steps + 1)
                }
                queue.add(Triple(next, steps + 1, nextComputer))
            }
        }
    }
    return null
}

data class Point(val x: Int, val y: Int) {
    fun move(direction: Int): Point {
        return when (direction) {
            1 -> Point(x, y - 1)
            2 -> Point(x, y + 1)
            3 -> Point(x - 1, y)
            4 -> Point(x + 1, y)
            else -> throw IllegalArgumentException("Invalid direction")
        }
    }
}

class IntcodeComputer(program: LongArray) {
    private val memory = program.toMutableList()
    private var ip = 0
    private var relativeBase = 0
    val input = LinkedList<Long>()
    val output = LinkedList<Long>()
    var halted = false

    init {
        for (i in 0 until 1000) {
            memory.add(0)
        }
    }

    fun run() {
        while (!halted) {
            val opcode = memory[ip] % 100
            when (opcode) {
                1L -> {
                    val p1 = getParam(1)
                    val p2 = getParam(2)
                    setParam(3, p1 + p2)
                    ip += 4
                }
                2L -> {
                    val p1 = getParam(1)
                    val p2 = getParam(2)
                    setParam(3, p1 * p2)
                    ip += 4
                }
                3L -> {
                    if (input.isEmpty()) return
                    setParam(1, input.poll())
                    ip += 2
                }
                4L -> {
                    output.add(getParam(1))
                    ip += 2
                }
                5L -> {
                    val p1 = getParam(1)
                    val p2 = getParam(2)
                    if (p1 != 0L) ip = p2.toInt() else ip += 3
                }
                6L -> {
                    val p1 = getParam(1)
                    val p2 = getParam(2)
                    if (p1 == 0L) ip = p2.toInt() else ip += 3
                }
                7L -> {
                    val p1 = getParam(1)
                    val p2 = getParam(2)
                    setParam(3, if (p1 < p2) 1L else 0L)
                    ip += 4
                }
                8L -> {
                    val p1 = getParam(1)
                    val p2 = getParam(2)
                    setParam(3, if (p1 == p2) 1L else 0L)
                    ip += 4
                }
                9L -> {
                    relativeBase += getParam(1).toInt()
                    ip += 2
                }
                99L -> {
                    halted = true
                }
                else -> throw IllegalStateException("Unknown opcode: $opcode")
            }
        }
    }

    private fun getParam(paramIndex: Int): Long {
        val mode = (memory[ip] / (10 * 10.0.pow(paramIndex.toDouble()))).toInt() % 10
        return when (mode) {
            0 -> memory[memory[ip + paramIndex].toInt()]
            1 -> memory[ip + paramIndex]
            2 -> memory[memory[ip + paramIndex].toInt() + relativeBase]
            else -> throw IllegalStateException("Unknown parameter mode: $mode")
        }
    }

    private fun setParam(paramIndex: Int, value: Long) {
        val mode = (memory[ip] / (10 * 10.0.pow(paramIndex.toDouble()))).toInt() % 10
        when (mode) {
            0 -> memory[memory[ip + paramIndex].toInt()] = value
            2 -> memory[memory[ip + paramIndex].toInt() + relativeBase] = value
            else -> throw IllegalStateException("Unknown parameter mode: $mode")
        }
    }

    fun copy(): IntcodeComputer {
        val newComputer = IntcodeComputer(memory.toLongArray())
        newComputer.ip = ip
        newComputer.relativeBase = relativeBase
        newComputer.input.addAll(input)
        newComputer.output.addAll(output)
        return newComputer
    }

    private fun Double.pow(exponent: Double): Double = Math.pow(this, exponent)
}
