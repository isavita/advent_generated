
import java.io.File

class IntcodeComputer(program: List<Long>) {
    private val memory = program.withIndex().associate { it.index.toLong() to it.value }.toMutableMap()
    private var ip = 0L
    private var relativeBase = 0L
    var halted = false

    private fun getParameter(mode: Int, offset: Int): Long {
        val param = memory.getOrDefault(ip + offset, 0L)
        return when (mode) {
            0 -> memory.getOrDefault(param, 0L)
            1 -> param
            2 -> memory.getOrDefault(relativeBase + param, 0L)
            else -> throw IllegalArgumentException("Unknown parameter mode: $mode")
        }
    }

    private fun setParameter(mode: Int, offset: Int, value: Long) {
        val param = memory.getOrDefault(ip + offset, 0L)
        when (mode) {
            0 -> memory[param] = value
            2 -> memory[relativeBase + param] = value
            else -> throw IllegalArgumentException("Unknown parameter mode for writing: $mode")
        }
    }

    fun run(inputProvider: () -> Long): Sequence<Long> = sequence {
        while (true) {
            val instruction = memory.getOrDefault(ip, 0L)
            val opcode = (instruction % 100).toInt()
            val modes = listOf(
                (instruction / 100 % 10).toInt(),
                (instruction / 1000 % 10).toInt(),
                (instruction / 10000 % 10).toInt()
            )

            when (opcode) {
                1 -> {
                    setParameter(modes[2], 3, getParameter(modes[0], 1) + getParameter(modes[1], 2))
                    ip += 4
                }
                2 -> {
                    setParameter(modes[2], 3, getParameter(modes[0], 1) * getParameter(modes[1], 2))
                    ip += 4
                }
                3 -> {
                    setParameter(modes[0], 1, inputProvider())
                    ip += 2
                }
                4 -> {
                    yield(getParameter(modes[0], 1))
                    ip += 2
                }
                5 -> {
                    ip = if (getParameter(modes[0], 1) != 0L) getParameter(modes[1], 2) else ip + 3
                }
                6 -> {
                    ip = if (getParameter(modes[0], 1) == 0L) getParameter(modes[1], 2) else ip + 3
                }
                7 -> {
                    setParameter(modes[2], 3, if (getParameter(modes[0], 1) < getParameter(modes[1], 2)) 1L else 0L)
                    ip += 4
                }
                8 -> {
                    setParameter(modes[2], 3, if (getParameter(modes[0], 1) == getParameter(modes[1], 2)) 1L else 0L)
                    ip += 4
                }
                9 -> {
                    relativeBase += getParameter(modes[0], 1)
                    ip += 2
                }
                99 -> {
                    halted = true
                    return@sequence
                }
                else -> throw IllegalArgumentException("Unknown opcode: $opcode")
            }
        }
    }
}

class Robot(program: List<Long>, startPanelColor: Int = 0) {
    private val computer = IntcodeComputer(program)
    private var direction = 0
    private var position = 0 to 0
    private val panels = mutableMapOf(position to startPanelColor)
    val paintedPanels = mutableSetOf<Pair<Int, Int>>()

    private fun turnAndMove(turnDirection: Int) {
        direction = (direction + if (turnDirection == 0) -1 else 1).mod(4)
        position = when (direction) {
            0 -> position.first to position.second - 1
            1 -> position.first + 1 to position.second
            2 -> position.first to position.second + 1
            3 -> position.first - 1 to position.second
            else -> throw IllegalArgumentException("Unknown direction: $direction")
        }
    }

    fun run() {
        val programOutput = computer.run { panels.getOrDefault(position, 0).toLong() }.iterator()
        while (!computer.halted && programOutput.hasNext()) {
            val paintColor = programOutput.next().toInt()
            if (!programOutput.hasNext()) break
            val turnDirection = programOutput.next().toInt()
            panels[position] = paintColor
            paintedPanels.add(position)
            turnAndMove(turnDirection)
        }
    }

    fun getPaintedPanelsCount() = paintedPanels.size

    fun renderPanels() {
        if (panels.isEmpty()) {
            println("No panels painted.")
            return
        }

        val minX = panels.keys.minOf { it.first }
        val maxX = panels.keys.maxOf { it.first }
        val minY = panels.keys.minOf { it.second }
        val maxY = panels.keys.maxOf { it.second }

        val grid = (minY..maxY).map { y ->
            (minX..maxX).map { x ->
                if (panels.getOrDefault(x to y, 0) == 1) '#' else ' '
            }.joinToString("")
        }
        println("\nRegistration Identifier:")
        grid.forEach { println(it) }
    }
}

fun parseInput(filePath: String) = File(filePath).readText().trim().split(',').map { it.toLong() }

fun main() {
    val program = parseInput("input.txt")

    val robotPart1 = Robot(program, 0)
    robotPart1.run()
    println("Part One: ${robotPart1.getPaintedPanelsCount()} panels painted at least once.")

    val robotPart2 = Robot(program, 1)
    robotPart2.run()
    println("Part Two: Registration identifier painted on the hull.")
    robotPart2.renderPanels()
}
