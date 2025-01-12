
import java.io.File
import java.math.BigInteger

fun main() {
    val program = File("input.txt").readText().trim().split(",").map { BigInteger(it) }
    val hull = mutableMapOf<Pair<Int, Int>, Int>()
    var x = 0
    var y = 0
    var direction = 0 // 0: up, 1: right, 2: down, 3: left
    val computer = IntcodeComputer(program)

    while (!computer.halted) {
        val panelColor = hull.getOrDefault(Pair(x, y), 0)
        computer.addInput(BigInteger.valueOf(panelColor.toLong()))
        computer.run()

        if (computer.output.size >= 2) {
            val paintColor = computer.output.removeFirst().toInt()
            val turnDirection = computer.output.removeFirst().toInt()

            hull[Pair(x, y)] = paintColor

            direction = when (turnDirection) {
                0 -> (direction + 3) % 4 // Turn left
                1 -> (direction + 1) % 4 // Turn right
                else -> throw IllegalArgumentException("Invalid turn direction: $turnDirection")
            }

            when (direction) {
                0 -> y--
                1 -> x++
                2 -> y++
                3 -> x--
            }
        }
    }

    println(hull.size)
}


class IntcodeComputer(program: List<BigInteger>) {
    private var memory = program.toMutableList()
    private var instructionPointer = 0
    private var relativeBase = 0
    val output = mutableListOf<BigInteger>()
    var halted = false
    private val input = mutableListOf<BigInteger>()

    fun addInput(value: BigInteger) {
        input.add(value)
    }

    fun run() {
        while (!halted) {
            val instruction = memory[instructionPointer].toInt()
            val opcode = instruction % 100
            val modes = (instruction / 100).toString().padStart(5, '0').reversed()

            when (opcode) {
                1 -> { // Add
                    val param1 = getParameter(instructionPointer + 1, modes[0])
                    val param2 = getParameter(instructionPointer + 2, modes[1])
                    val param3 = getAddress(instructionPointer + 3, modes[2])
                    memory[param3] = param1 + param2
                    instructionPointer += 4
                }
                2 -> { // Multiply
                    val param1 = getParameter(instructionPointer + 1, modes[0])
                    val param2 = getParameter(instructionPointer + 2, modes[1])
                    val param3 = getAddress(instructionPointer + 3, modes[2])
                    memory[param3] = param1 * param2
                    instructionPointer += 4
                }
                3 -> { // Input
                    if (input.isEmpty()) return
                    val param1 = getAddress(instructionPointer + 1, modes[0])
                    memory[param1] = input.removeFirst()
                    instructionPointer += 2
                }
                4 -> { // Output
                    val param1 = getParameter(instructionPointer + 1, modes[0])
                    output.add(param1)
                    instructionPointer += 2
                }
                5 -> { // Jump-if-true
                    val param1 = getParameter(instructionPointer + 1, modes[0])
                    val param2 = getParameter(instructionPointer + 2, modes[1])
                    if (param1 != BigInteger.ZERO) {
                        instructionPointer = param2.toInt()
                    } else {
                        instructionPointer += 3
                    }
                }
                6 -> { // Jump-if-false
                    val param1 = getParameter(instructionPointer + 1, modes[0])
                    val param2 = getParameter(instructionPointer + 2, modes[1])
                    if (param1 == BigInteger.ZERO) {
                        instructionPointer = param2.toInt()
                    } else {
                        instructionPointer += 3
                    }
                }
                7 -> { // Less than
                    val param1 = getParameter(instructionPointer + 1, modes[0])
                    val param2 = getParameter(instructionPointer + 2, modes[1])
                    val param3 = getAddress(instructionPointer + 3, modes[2])
                    memory[param3] = if (param1 < param2) BigInteger.ONE else BigInteger.ZERO
                    instructionPointer += 4
                }
                8 -> { // Equals
                    val param1 = getParameter(instructionPointer + 1, modes[0])
                    val param2 = getParameter(instructionPointer + 2, modes[1])
                    val param3 = getAddress(instructionPointer + 3, modes[2])
                    memory[param3] = if (param1 == param2) BigInteger.ONE else BigInteger.ZERO
                    instructionPointer += 4
                }
                9 -> { // Adjust relative base
                    val param1 = getParameter(instructionPointer + 1, modes[0])
                    relativeBase += param1.toInt()
                    instructionPointer += 2
                }
                99 -> { // Halt
                    halted = true
                    return
                }
                else -> throw IllegalArgumentException("Unknown opcode: $opcode")
            }
        }
    }

    private fun getParameter(address: Int, mode: Char): BigInteger {
        val value = memory.getOrElse(address) { BigInteger.ZERO }
        return when (mode) {
            '0' -> memory.getOrElse(value.toInt()) { BigInteger.ZERO }
            '1' -> value
            '2' -> memory.getOrElse(relativeBase + value.toInt()) { BigInteger.ZERO }
            else -> throw IllegalArgumentException("Invalid mode: $mode")
        }
    }

    private fun getAddress(address: Int, mode: Char): Int {
        val value = memory.getOrElse(address) { BigInteger.ZERO }
        return when (mode) {
            '0' -> value.toInt()
            '2' -> relativeBase + value.toInt()
            else -> throw IllegalArgumentException("Invalid mode: $mode")
        }
    }
}
