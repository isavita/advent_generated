import java.io.File

fun main() {
    val intcodeProgram = File("input.txt").readLines().first().split(",").map { it.toInt() }
    val phaseSettings = listOf(0, 1, 2, 3, 4)
    val permutations = phaseSettings.toMutableList().permutations()

    var maxThrusterSignal = 0

    for (permutation in permutations) {
        var inputSignal = 0
        for (phaseSetting in permutation) {
            inputSignal = runAmplifier(intcodeProgram, phaseSetting, inputSignal)
        }
        if (inputSignal > maxThrusterSignal) {
            maxThrusterSignal = inputSignal
        }
    }

    println("The highest signal that can be sent to the thrusters is $maxThrusterSignal")
}

fun runAmplifier(program: List<Int>, phaseSetting: Int, inputSignal: Int): Int {
    val memory = program.toMutableList()
    var pointer = 0
    var output = 0
    var inputIndex = 0
    val inputs = listOf(phaseSetting, inputSignal)

    while (true) {
        val opcode = memory[pointer] % 100
        val mode1 = memory[pointer] / 100 % 10
        val mode2 = memory[pointer] / 1000 % 10

        when (opcode) {
            1 -> {
                val param1 = if (mode1 == 0) memory[memory[pointer + 1]] else memory[pointer + 1]
                val param2 = if (mode2 == 0) memory[memory[pointer + 2]] else memory[pointer + 2]
                memory[memory[pointer + 3]] = param1 + param2
                pointer += 4
            }
            2 -> {
                val param1 = if (mode1 == 0) memory[memory[pointer + 1]] else memory[pointer + 1]
                val param2 = if (mode2 == 0) memory[memory[pointer + 2]] else memory[pointer + 2]
                memory[memory[pointer + 3]] = param1 * param2
                pointer += 4
            }
            3 -> {
                memory[memory[pointer + 1]] = inputs[inputIndex++]
                pointer += 2
            }
            4 -> {
                output = if (mode1 == 0) memory[memory[pointer + 1]] else memory[pointer + 1]
                pointer += 2
            }
            5 -> {
                val param1 = if (mode1 == 0) memory[memory[pointer + 1]] else memory[pointer + 1]
                val param2 = if (mode2 == 0) memory[memory[pointer + 2]] else memory[pointer + 2]
                if (param1 != 0) {
                    pointer = param2
                } else {
                    pointer += 3
                }
            }
            6 -> {
                val param1 = if (mode1 == 0) memory[memory[pointer + 1]] else memory[pointer + 1]
                val param2 = if (mode2 == 0) memory[memory[pointer + 2]] else memory[pointer + 2]
                if (param1 == 0) {
                    pointer = param2
                } else {
                    pointer += 3
                }
            }
            7 -> {
                val param1 = if (mode1 == 0) memory[memory[pointer + 1]] else memory[pointer + 1]
                val param2 = if (mode2 == 0) memory[memory[pointer + 2]] else memory[pointer + 2]
                memory[memory[pointer + 3]] = if (param1 < param2) 1 else 0
                pointer += 4
            }
            8 -> {
                val param1 = if (mode1 == 0) memory[memory[pointer + 1]] else memory[pointer + 1]
                val param2 = if (mode2 == 0) memory[memory[pointer + 2]] else memory[pointer + 2]
                memory[memory[pointer + 3]] = if (param1 == param2) 1 else 0
                pointer += 4
            }
            99 -> break
            else -> throw IllegalArgumentException("Unknown opcode $opcode")
        }
    }

    return output
}

fun <T> List<T>.permutations(): List<List<T>> {
    if (this.size == 1) return listOf(this)
    val perms = mutableListOf<List<T>>()
    val toInsert = this[0]
    for (perm in this.drop(1).permutations()) {
        for (i in 0..perm.size) {
            val newPerm = perm.toMutableList()
            newPerm.add(i, toInsert)
            perms.add(newPerm)
        }
    }
    return perms
}