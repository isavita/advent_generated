
import java.io.File
import java.util.concurrent.LinkedBlockingQueue

fun main() {
    val program = File("input.txt").readText().trim().split(",").map { it.toInt() }

    fun runAmplifier(program: List<Int>, phase: Int, inputQueue: LinkedBlockingQueue<Int>, outputQueue: LinkedBlockingQueue<Int>) {
        val memory = program.toMutableList()
        var instructionPointer = 0
        var phaseSet = false

        while (instructionPointer < memory.size) {
            val opcode = memory[instructionPointer] % 100
            val paramModes = memory[instructionPointer] / 100
            
            fun getParam(offset: Int, mode: Int): Int {
                val value = memory[instructionPointer + offset]
                return when (mode % 10) {
                    0 -> if (value < memory.size) memory[value] else 0
                    1 -> value
                    else -> throw IllegalArgumentException("Invalid parameter mode: $mode")
                }
            }

            when (opcode) {
                1 -> { // Add
                    val param1 = getParam(1, paramModes / 1)
                    val param2 = getParam(2, paramModes / 10)
                    val dest = memory[instructionPointer + 3]
                    memory[dest] = param1 + param2
                    instructionPointer += 4
                }
                2 -> { // Multiply
                    val param1 = getParam(1, paramModes / 1)
                    val param2 = getParam(2, paramModes / 10)
                    val dest = memory[instructionPointer + 3]
                    memory[dest] = param1 * param2
                    instructionPointer += 4
                }
                3 -> { // Input
                    val dest = memory[instructionPointer + 1]
                    val input = if (!phaseSet) {
                        phaseSet = true
                        phase
                    } else {
                        inputQueue.take()
                    }
                    memory[dest] = input
                    instructionPointer += 2
                }
                4 -> { // Output
                    val output = getParam(1, paramModes / 1)
                    outputQueue.put(output)
                    instructionPointer += 2
                }
                5 -> { // Jump-if-true
                    val param1 = getParam(1, paramModes / 1)
                    val param2 = getParam(2, paramModes / 10)
                    instructionPointer = if (param1 != 0) param2 else instructionPointer + 3
                }
                6 -> { // Jump-if-false
                    val param1 = getParam(1, paramModes / 1)
                    val param2 = getParam(2, paramModes / 10)
                    instructionPointer = if (param1 == 0) param2 else instructionPointer + 3
                }
                7 -> { // Less than
                    val param1 = getParam(1, paramModes / 1)
                    val param2 = getParam(2, paramModes / 10)
                    val dest = memory[instructionPointer + 3]
                    memory[dest] = if (param1 < param2) 1 else 0
                    instructionPointer += 4
                }
                8 -> { // Equals
                    val param1 = getParam(1, paramModes / 1)
                    val param2 = getParam(2, paramModes / 10)
                    val dest = memory[instructionPointer + 3]
                    memory[dest] = if (param1 == param2) 1 else 0
                    instructionPointer += 4
                }
                99 -> break // Halt
                else -> throw IllegalArgumentException("Unknown opcode: $opcode")
            }
        }
    }

    fun calculateThrusterSignal(phases: List<Int>, feedbackMode: Boolean): Int {
        val queues = List(5) { LinkedBlockingQueue<Int>() }
        val threads = List(5) { i ->
            Thread {
                runAmplifier(program, phases[i], queues[i], queues[(i + 1) % 5])
            }
        }

        threads.forEach { it.start() }
        queues[0].put(0)

        if (feedbackMode) {
            threads.forEach { it.join() }
        } else {
            threads.forEach { it.join() }
        }

        return queues[0].poll() ?: 0
    }

    fun generatePermutations(elements: List<Int>): List<List<Int>> {
        if (elements.isEmpty()) return listOf(emptyList())
        val permutations = mutableListOf<List<Int>>()
        for (i in elements.indices) {
            val remaining = elements.filterIndexed { index, _ -> index != i }
            val subPermutations = generatePermutations(remaining)
            for (subPermutation in subPermutations) {
                permutations.add(listOf(elements[i]) + subPermutation)
            }
        }
        return permutations
    }

    val part1Phases = (0..4).toList()
    val part1MaxSignal = generatePermutations(part1Phases)
        .maxOf { calculateThrusterSignal(it, false) }
    println("Part 1 Max Thruster Signal: $part1MaxSignal")

    val part2Phases = (5..9).toList()
    val part2MaxSignal = generatePermutations(part2Phases)
        .maxOf { calculateThrusterSignal(it, true) }
    println("Part 2 Max Thruster Signal: $part2MaxSignal")
}
