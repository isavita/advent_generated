import java.io.File
import java.util.ArrayDeque

// Data class to hold compressed movement routines.
data class Quadruple(val mainRoutine: String, val functionA: String, val functionB: String, val functionC: String)

class IntcodeComputer(program: List<Long>) {
    private val memory: MutableMap<Int, Long> = mutableMapOf()
    var pointer = 0
    var relativeBase = 0
    val inputs: ArrayDeque<Long> = ArrayDeque()
    val outputs: ArrayDeque<Long> = ArrayDeque()
    var halted = false

    init {
        program.forEachIndexed { index, value ->
            memory[index] = value
        }
    }

    private fun getMemory(index: Int): Long = memory.getOrDefault(index, 0L)
    private fun setMemory(index: Int, value: Long) { memory[index] = value }

    private fun getParam(mode: Int, param: Int): Long = when (mode) {
        0 -> getMemory(param)
        1 -> param.toLong()
        2 -> getMemory(relativeBase + param)
        else -> throw Exception("Unknown parameter mode: $mode")
    }

    private fun setParam(mode: Int, param: Int, value: Long) {
        when (mode) {
            0 -> setMemory(param, value)
            2 -> setMemory(relativeBase + param, value)
            else -> throw Exception("Unknown parameter mode for writing: $mode")
        }
    }

    fun run() {
        while (true) {
            val instruction = getMemory(pointer).toInt()
            val opcode = instruction % 100
            val modes = listOf(
                (instruction / 100) % 10,
                (instruction / 1000) % 10,
                (instruction / 10000) % 10
            )
            when (opcode) {
                1 -> { // Addition
                    val param1 = getMemory(pointer + 1).toInt()
                    val param2 = getMemory(pointer + 2).toInt()
                    val param3 = getMemory(pointer + 3).toInt()
                    val val1 = getParam(modes[0], param1)
                    val val2 = getParam(modes[1], param2)
                    setParam(modes[2], param3, val1 + val2)
                    pointer += 4
                }
                2 -> { // Multiplication
                    val param1 = getMemory(pointer + 1).toInt()
                    val param2 = getMemory(pointer + 2).toInt()
                    val param3 = getMemory(pointer + 3).toInt()
                    val val1 = getParam(modes[0], param1)
                    val val2 = getParam(modes[1], param2)
                    setParam(modes[2], param3, val1 * val2)
                    pointer += 4
                }
                3 -> { // Input
                    if (inputs.isEmpty()) return
                    val param1 = getMemory(pointer + 1).toInt()
                    val inputValue = inputs.removeFirst()
                    setParam(modes[0], param1, inputValue)
                    pointer += 2
                }
                4 -> { // Output
                    val param1 = getMemory(pointer + 1).toInt()
                    val outputValue = getParam(modes[0], param1)
                    outputs.addLast(outputValue)
                    pointer += 2
                }
                5 -> { // Jump-if-true
                    val param1 = getMemory(pointer + 1).toInt()
                    val param2 = getMemory(pointer + 2).toInt()
                    val val1 = getParam(modes[0], param1)
                    val val2 = getParam(modes[1], param2)
                    pointer = if (val1 != 0L) val2.toInt() else pointer + 3
                }
                6 -> { // Jump-if-false
                    val param1 = getMemory(pointer + 1).toInt()
                    val param2 = getMemory(pointer + 2).toInt()
                    val val1 = getParam(modes[0], param1)
                    val val2 = getParam(modes[1], param2)
                    pointer = if (val1 == 0L) val2.toInt() else pointer + 3
                }
                7 -> { // Less than
                    val param1 = getMemory(pointer + 1).toInt()
                    val param2 = getMemory(pointer + 2).toInt()
                    val param3 = getMemory(pointer + 3).toInt()
                    val val1 = getParam(modes[0], param1)
                    val val2 = getParam(modes[1], param2)
                    setParam(modes[2], param3, if (val1 < val2) 1L else 0L)
                    pointer += 4
                }
                8 -> { // Equals
                    val param1 = getMemory(pointer + 1).toInt()
                    val param2 = getMemory(pointer + 2).toInt()
                    val param3 = getMemory(pointer + 3).toInt()
                    val val1 = getParam(modes[0], param1)
                    val val2 = getParam(modes[1], param2)
                    setParam(modes[2], param3, if (val1 == val2) 1L else 0L)
                    pointer += 4
                }
                9 -> { // Adjust relative base
                    val param1 = getMemory(pointer + 1).toInt()
                    val val1 = getParam(modes[0], param1)
                    relativeBase += val1.toInt()
                    pointer += 2
                }
                99 -> { // Halt
                    halted = true
                    return
                }
                else -> throw Exception("Unknown opcode: $opcode")
            }
        }
    }
}

fun readInput(filename: String): List<Long> {
    val content = File(filename).readText().trim()
    return content.split(",").map { it.toLong() }
}

fun parseMap(output: List<Long>): List<List<Char>> {
    val grid = mutableListOf<List<Char>>()
    var line = mutableListOf<Char>()
    for (c in output) {
        if (c == 10L) {
            if (line.isNotEmpty()) {
                grid.add(line.toList())
                line = mutableListOf()
            }
        } else {
            line.add(c.toInt().toChar())
        }
    }
    if (line.isNotEmpty()) grid.add(line)
    return grid
}

fun findIntersections(grid: List<List<Char>>): List<Pair<Int, Int>> {
    val intersections = mutableListOf<Pair<Int, Int>>()
    for (y in 1 until grid.size - 1) {
        for (x in 1 until grid[0].size - 1) {
            if (grid[y][x] != '#') continue
            if (grid[y - 1][x] == '#' &&
                grid[y + 1][x] == '#' &&
                grid[y][x - 1] == '#' &&
                grid[y][x + 1] == '#') {
                intersections.add(Pair(x, y))
            }
        }
    }
    return intersections
}

fun findRobotPosition(grid: List<List<Char>>): Triple<Int, Int, Char>? {
    for ((y, row) in grid.withIndex()) {
        for ((x, cell) in row.withIndex()) {
            if (cell in listOf('^', 'v', '<', '>', 'X')) {
                return Triple(x, y, cell)
            }
        }
    }
    return null
}

fun turnLeft(direction: Char): Char = when (direction) {
    '^' -> '<'
    '<' -> 'v'
    'v' -> '>'
    '>' -> '^'
    else -> throw Exception("Unknown direction: $direction")
}

fun turnRight(direction: Char): Char = when (direction) {
    '^' -> '>'
    '>' -> 'v'
    'v' -> '<'
    '<' -> '^'
    else -> throw Exception("Unknown direction: $direction")
}

fun moveForward(x: Int, y: Int, direction: Char): Pair<Int, Int> = when (direction) {
    '^' -> Pair(x, y - 1)
    'v' -> Pair(x, y + 1)
    '<' -> Pair(x - 1, y)
    '>' -> Pair(x + 1, y)
    else -> throw Exception("Unknown direction: $direction")
}

fun getMovementPath(grid: List<List<Char>>, startX: Int, startY: Int, startDir: Char): List<String> {
    var x = startX
    var y = startY
    var direction = startDir
    val path = mutableListOf<String>()
    var steps = 0
    while (true) {
        val (nextX, nextY) = moveForward(x, y, direction)
        if (nextY in grid.indices && nextX in grid[0].indices && grid[nextY][nextX] == '#') {
            x = nextX
            y = nextY
            steps++
        } else {
            if (steps > 0) {
                path.add(steps.toString())
                steps = 0
            }
            val leftDir = turnLeft(direction)
            val (lx, ly) = moveForward(x, y, leftDir)
            if (ly in grid.indices && lx in grid[0].indices && grid[ly][lx] == '#') {
                path.add("L")
                direction = leftDir
                continue
            }
            val rightDir = turnRight(direction)
            val (rx, ry) = moveForward(x, y, rightDir)
            if (ry in grid.indices && rx in grid[0].indices && grid[ry][rx] == '#') {
                path.add("R")
                direction = rightDir
                continue
            }
            break
        }
    }
    return path
}

fun replaceSequence(seq: List<String>, pattern: List<String>, replacement: String): List<String> {
    var i = 0
    val res = mutableListOf<String>()
    while (i < seq.size) {
        if (i + pattern.size <= seq.size && seq.subList(i, i + pattern.size) == pattern) {
            res.add(replacement)
            i += pattern.size
        } else {
            res.add(seq[i])
            i++
        }
    }
    return res
}

fun compressMovement(path: List<String>): Quadruple {
    val maxFunctionLength = 20  // max characters per function
    val maxPatternLength = 10   // max tokens per function

    val tokens = path.joinToString(",").split(",")
    for (aLen in 1..maxPatternLength) {
        val aPattern = tokens.take(aLen)
        val aStr = aPattern.joinToString(",")
        if (aStr.length > maxFunctionLength) continue
        val tokensAfterA = replaceSequence(tokens, aPattern, "A")
        for (bStart in aLen until tokens.size) {
            for (bLen in 1..maxPatternLength) {
                if (bStart + bLen > tokens.size) break
                val bPattern = tokens.subList(bStart, bStart + bLen)
                val bStr = bPattern.joinToString(",")
                if (bStr.length > maxFunctionLength) continue
                val tokensAfterB = replaceSequence(tokensAfterA, bPattern, "B")
                for (cStart in bStart + bLen until tokens.size) {
                    for (cLen in 1..maxPatternLength) {
                        if (cStart + cLen > tokens.size) break
                        val cPattern = tokens.subList(cStart, cStart + cLen)
                        val cStr = cPattern.joinToString(",")
                        if (cStr.length > maxFunctionLength) continue
                        val tokensAfterC = replaceSequence(tokensAfterB, cPattern, "C")
                        var mainTokens = tokensAfterC.toMutableList()
                        var changed = true
                        while (changed) {
                            changed = false
                            val tempTokens = mainTokens.toList()
                            mainTokens = mutableListOf()
                            var i = 0
                            while (i < tempTokens.size) {
                                when {
                                    i + aPattern.size <= tempTokens.size && tempTokens.subList(i, i + aPattern.size) == aPattern -> {
                                        mainTokens.add("A")
                                        i += aPattern.size
                                        changed = true
                                    }
                                    i + bPattern.size <= tempTokens.size && tempTokens.subList(i, i + bPattern.size) == bPattern -> {
                                        mainTokens.add("B")
                                        i += bPattern.size
                                        changed = true
                                    }
                                    i + cPattern.size <= tempTokens.size && tempTokens.subList(i, i + cPattern.size) == cPattern -> {
                                        mainTokens.add("C")
                                        i += cPattern.size
                                        changed = true
                                    }
                                    else -> {
                                        mainTokens.add(tempTokens[i])
                                        i++
                                    }
                                }
                            }
                        }
                        val mainRoutine = mainTokens.joinToString(",")
                        if (mainRoutine.all { it in "ABC," } && mainRoutine.length <= maxFunctionLength) {
                            val functionA = aPattern.joinToString(",")
                            val functionB = bPattern.joinToString(",")
                            val functionC = cPattern.joinToString(",")
                            if (functionA.length <= maxFunctionLength &&
                                functionB.length <= maxFunctionLength &&
                                functionC.length <= maxFunctionLength) {
                                return Quadruple(mainRoutine, functionA, functionB, functionC)
                            }
                        }
                    }
                }
            }
        }
    }
    throw Exception("Could not compress the path into functions A, B, C.")
}

fun main() {
    // Read Intcode program from input.txt
    val program = readInput("input.txt")

    // ----- Part One -----
    val computer = IntcodeComputer(program)
    computer.run()
    val output = computer.outputs.toList()
    val grid = parseMap(output)
    val intersections = findIntersections(grid)
    val alignmentSum = intersections.sumOf { it.first * it.second }
    println("Part One: Sum of alignment parameters = $alignmentSum")

    // ----- Part Two -----
    val programPart2 = program.toMutableList()
    programPart2[0] = 2L
    val computerPart2 = IntcodeComputer(programPart2)

    val robot = findRobotPosition(grid) ?: throw Exception("Robot not found on the scaffold.")
    val (startX, startY, startDir) = robot

    val movementPath = getMovementPath(grid, startX, startY, startDir)
    val (mainRoutine, functionA, functionB, functionC) = compressMovement(movementPath)

    // Prepare input lines, each ending with a newline (ASCII 10)
    val inputLines = listOf(mainRoutine, functionA, functionB, functionC, "n")
    val movementInputs = mutableListOf<Long>()
    for (line in inputLines) {
        for (char in line) {
            movementInputs.add(char.code.toLong())
        }
        movementInputs.add(10L)
    }

    computerPart2.inputs.addAll(movementInputs)

    while (!computerPart2.halted) {
        computerPart2.run()
    }

    var dustCollected = 0L
    while (computerPart2.outputs.isNotEmpty()) {
        dustCollected = computerPart2.outputs.removeFirst()
    }

    println("Part Two: Dust collected = $dustCollected")
}
