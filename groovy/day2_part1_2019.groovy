def program = new File("input.txt").text.tokenize(',').collect { it.toInteger() }
program[1] = 12
program[2] = 2

int pos = 0
while (program[pos] != 99) {
    def opcode = program[pos]
    def param1 = program[program[pos + 1]]
    def param2 = program[program[pos + 2]]
    def dest = program[pos + 3]

    if (opcode == 1) {
        program[dest] = param1 + param2
    } else if (opcode == 2) {
        program[dest] = param1 * param2
    }

    pos += 4
}

println program[0]