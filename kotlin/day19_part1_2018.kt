import java.io.File

data class Instruction(val opcode: String, val a: Int, val b: Int, val c: Int)

fun main() {
    val instructions = mutableListOf<Instruction>()
    var ipRegister = 0

    // Read the input file
    File("input.txt").forEachLine { line ->
        if (line.startsWith("#ip")) {
            ipRegister = line.split(" ")[1].toInt()
        } else {
            val parts = line.split(" ")
            instructions.add(Instruction(parts[0], parts[1].toInt(), parts[2].toInt(), parts[3].toInt()))
        }
    }

    val registers = IntArray(6)
    var ip = 0

    // Function to execute a single instruction
    fun executeInstruction(instruction: Instruction) {
        when (instruction.opcode) {
            "addr" -> registers[instruction.c] = registers[instruction.a] + registers[instruction.b]
            "addi" -> registers[instruction.c] = registers[instruction.a] + instruction.b
            "mulr" -> registers[instruction.c] = registers[instruction.a] * registers[instruction.b]
            "muli" -> registers[instruction.c] = registers[instruction.a] * instruction.b
            "banr" -> registers[instruction.c] = registers[instruction.a] and registers[instruction.b]
            "bani" -> registers[instruction.c] = registers[instruction.a] and instruction.b
            "borr" -> registers[instruction.c] = registers[instruction.a] or registers[instruction.b]
            "bori" -> registers[instruction.c] = registers[instruction.a] or instruction.b
            "setr" -> registers[instruction.c] = registers[instruction.a]
            "seti" -> registers[instruction.c] = instruction.a
            "gtir" -> registers[instruction.c] = if (instruction.a > registers[instruction.b]) 1 else 0
            "gtri" -> registers[instruction.c] = if (registers[instruction.a] > instruction.b) 1 else 0
            "gtrr" -> registers[instruction.c] = if (registers[instruction.a] > registers[instruction.b]) 1 else 0
            "eqir" -> registers[instruction.c] = if (instruction.a == registers[instruction.b]) 1 else 0
            "eqri" -> registers[instruction.c] = if (registers[instruction.a] == instruction.b) 1 else 0
            "eqrr" -> registers[instruction.c] = if (registers[instruction.a] == registers[instruction.b]) 1 else 0
        }
    }

    // Execute the program
    while (ip in instructions.indices) {
        registers[ipRegister] = ip
        executeInstruction(instructions[ip])
        ip = registers[ipRegister] + 1
    }

    // Print the value in register 0
    println(registers[0])
}