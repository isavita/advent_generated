import java.io.File

fun main() {
    val input = File("input.txt").readText()
    println(solve(input))
}

fun solve(input: String): Int {
    val opcodeComputer = parseInput(input)

    while (!opcodeComputer.tick()) {
        if (opcodeComputer.registers[opcodeComputer.instructionPointer] == 28) {
            break
        }
    }

    return opcodeComputer.registers[5]
}

class OpcodeComputer(
    val instructions: List<Instruction>,
    var registers: IntArray,
    val instructionPointer: Int
) {
    fun tick(): Boolean {
        if (registers[instructionPointer] >= instructions.size) {
            println("Out of range instruction, terminating...")
            return true
        }
        val instIndex = registers[instructionPointer]
        val inst = instructions[instIndex]

        val opcodeFunc = opcodeNamesToFuncs[inst.name]!!

        registers = opcodeFunc(registers, inst.abcValues)

        registers[instructionPointer]++

        return registers[instructionPointer] >= instructions.size
    }
}

data class Instruction(val name: String, val abcValues: IntArray)

fun parseInput(input: String): OpcodeComputer {
    val lines = input.split("\n")

    val instructionPointer = lines[0].substringAfter("#ip ").toInt()

    val instructions = lines.drop(1).map { line ->
        val parts = line.split(" ")
        Instruction(parts[0], parts.drop(1).map { it.toInt() }.toIntArray())
    }

    return OpcodeComputer(instructions, IntArray(6), instructionPointer)
}

typealias OpcodeFunc = (IntArray, IntArray) -> IntArray

val opcodeNamesToFuncs = mapOf(
    "addr" to ::addr, "addi" to ::addi,
    "mulr" to ::mulr, "muli" to ::muli,
    "banr" to ::banr, "bani" to ::bani,
    "borr" to ::borr, "bori" to ::bori,
    "setr" to ::setr, "seti" to ::seti,
    "gtir" to ::gtir, "gtri" to ::gtri, "gtrr" to ::gtrr,
    "eqir" to ::eqir, "eqri" to ::eqri, "eqrr" to ::eqrr
)

fun addr(registers: IntArray, abcValues: IntArray): IntArray {
    registers[abcValues[2]] = registers[abcValues[0]] + registers[abcValues[1]]
    return registers
}

fun addi(registers: IntArray, abcValues: IntArray): IntArray {
    registers[abcValues[2]] = registers[abcValues[0]] + abcValues[1]
    return registers
}

fun mulr(registers: IntArray, abcValues: IntArray): IntArray {
    registers[abcValues[2]] = registers[abcValues[0]] * registers[abcValues[1]]
    return registers
}

fun muli(registers: IntArray, abcValues: IntArray): IntArray {
    registers[abcValues[2]] = registers[abcValues[0]] * abcValues[1]
    return registers
}

fun banr(registers: IntArray, abcValues: IntArray): IntArray {
    registers[abcValues[2]] = registers[abcValues[0]] and registers[abcValues[1]]
    return registers
}

fun bani(registers: IntArray, abcValues: IntArray): IntArray {
    registers[abcValues[2]] = registers[abcValues[0]] and abcValues[1]
    return registers
}

fun borr(registers: IntArray, abcValues: IntArray): IntArray {
    registers[abcValues[2]] = registers[abcValues[0]] or registers[abcValues[1]]
    return registers
}

fun bori(registers: IntArray, abcValues: IntArray): IntArray {
    registers[abcValues[2]] = registers[abcValues[0]] or abcValues[1]
    return registers
}

fun setr(registers: IntArray, abcValues: IntArray): IntArray {
    registers[abcValues[2]] = registers[abcValues[0]]
    return registers
}

fun seti(registers: IntArray, abcValues: IntArray): IntArray {
    registers[abcValues[2]] = abcValues[0]
    return registers
}

fun gtir(registers: IntArray, abcValues: IntArray): IntArray {
    registers[abcValues[2]] = if (abcValues[0] > registers[abcValues[1]]) 1 else 0
    return registers
}

fun gtri(registers: IntArray, abcValues: IntArray): IntArray {
    registers[abcValues[2]] = if (registers[abcValues[0]] > abcValues[1]) 1 else 0
    return registers
}

fun gtrr(registers: IntArray, abcValues: IntArray): IntArray {
    registers[abcValues[2]] = if (registers[abcValues[0]] > registers[abcValues[1]]) 1 else 0
    return registers
}

fun eqir(registers: IntArray, abcValues: IntArray): IntArray {
    registers[abcValues[2]] = if (abcValues[0] == registers[abcValues[1]]) 1 else 0
    return registers
}

fun eqri(registers: IntArray, abcValues: IntArray): IntArray {
    registers[abcValues[2]] = if (registers[abcValues[0]] == abcValues[1]) 1 else 0
    return registers
}

fun eqrr(registers: IntArray, abcValues: IntArray): IntArray {
    registers[abcValues[2]] = if (registers[abcValues[0]] == registers[abcValues[1]]) 1 else 0
    return registers
}