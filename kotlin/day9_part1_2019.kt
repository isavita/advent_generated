import java.io.File

fun main() {
    val program = File("input.txt").readLines().first().split(",").map { it.toLong() }
    val output = mutableListOf<Long>()
    val computer = IntcodeComputer(program)
    computer.run(1, output)
    println(output.last())
}

class IntcodeComputer(private val program: List<Long>) {
    private var memory = program.toMutableList()
    private var pointer = 0
    private var relativeBase = 0L

    fun run(input: Long, output: MutableList<Long>) {
        while (pointer < memory.size) {
            val instruction = memory[pointer].toString().padStart(5, '0')
            val opcode = instruction.takeLast(2).toInt()
            val modes = instruction.dropLast(2).reversed()

            when (opcode) {
                1 -> {
                    val param1 = getParameter(modes[0].digitToInt(), 1)
                    val param2 = getParameter(modes[1].digitToInt(), 2)
                    val param3 = getParameterAddress(modes[2].digitToInt(), 3)
                    ensureMemorySize(param3)
                    memory[param3] = param1 + param2
                    pointer += 4
                }
                2 -> {
                    val param1 = getParameter(modes[0].digitToInt(), 1)
                    val param2 = getParameter(modes[1].digitToInt(), 2)
                    val param3 = getParameterAddress(modes[2].digitToInt(), 3)
                    ensureMemorySize(param3)
                    memory[param3] = param1 * param2
                    pointer += 4
                }
                3 -> {
                    val param1 = getParameterAddress(modes[0].digitToInt(), 1)
                    ensureMemorySize(param1)
                    memory[param1] = input
                    pointer += 2
                }
                4 -> {
                    val param1 = getParameter(modes[0].digitToInt(), 1)
                    output.add(param1)
                    pointer += 2
                }
                5 -> {
                    val param1 = getParameter(modes[0].digitToInt(), 1)
                    val param2 = getParameter(modes[1].digitToInt(), 2)
                    if (param1 != 0L) pointer = param2.toInt() else pointer += 3
                }
                6 -> {
                    val param1 = getParameter(modes[0].digitToInt(), 1)
                    val param2 = getParameter(modes[1].digitToInt(), 2)
                    if (param1 == 0L) pointer = param2.toInt() else pointer += 3
                }
                7 -> {
                    val param1 = getParameter(modes[0].digitToInt(), 1)
                    val param2 = getParameter(modes[1].digitToInt(), 2)
                    val param3 = getParameterAddress(modes[2].digitToInt(), 3)
                    ensureMemorySize(param3)
                    memory[param3] = if (param1 < param2) 1 else 0
                    pointer += 4
                }
                8 -> {
                    val param1 = getParameter(modes[0].digitToInt(), 1)
                    val param2 = getParameter(modes[1].digitToInt(), 2)
                    val param3 = getParameterAddress(modes[2].digitToInt(), 3)
                    ensureMemorySize(param3)
                    memory[param3] = if (param1 == param2) 1 else 0
                    pointer += 4
                }
                9 -> {
                    val param1 = getParameter(modes[0].digitToInt(), 1)
                    relativeBase += param1
                    pointer += 2
                }
                99 -> break
            }
        }
    }

    private fun getParameter(mode: Int, offset: Int): Long {
        return when (mode) {
            0 -> memory.getOrElse(memory[pointer + offset].toInt()) { 0L }
            1 -> memory[pointer + offset]
            2 -> memory.getOrElse((memory[pointer + offset] + relativeBase).toInt()) { 0L }
            else -> throw IllegalArgumentException("Invalid parameter mode: $mode")
        }
    }

    private fun getParameterAddress(mode: Int, offset: Int): Int {
        return when (mode) {
            0 -> memory[pointer + offset].toInt()
            2 -> (memory[pointer + offset] + relativeBase).toInt()
            else -> throw IllegalArgumentException("Invalid parameter mode for address: $mode")
        }
    }

    private fun ensureMemorySize(index: Int) {
        if (index >= memory.size) {
            memory.addAll(List(index - memory.size + 1) { 0L })
        }
    }
}