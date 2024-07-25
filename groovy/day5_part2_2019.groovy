
def getValue(program, pos, mode) {
    mode == 0 ? program[program[pos]] : program[pos]
}

def runProgram(program, input) {
    int i = 0
    while (true) {
        int opcode = program[i] % 100
        int modes = program[i] / 100
        int param1Mode = modes % 10
        modes /= 10
        int param2Mode = modes % 10

        switch (opcode) {
            case 1:
                program[program[i + 3]] = getValue(program, i + 1, param1Mode) + getValue(program, i + 2, param2Mode)
                i += 4
                break
            case 2:
                program[program[i + 3]] = getValue(program, i + 1, param1Mode) * getValue(program, i + 2, param2Mode)
                i += 4
                break
            case 3:
                program[program[i + 1]] = input
                i += 2
                break
            case 4:
                println getValue(program, i + 1, param1Mode)
                i += 2
                break
            case 5:
                i = getValue(program, i + 1, param1Mode) != 0 ? getValue(program, i + 2, param2Mode) : i + 3
                break
            case 6:
                i = getValue(program, i + 1, param1Mode) == 0 ? getValue(program, i + 2, param2Mode) : i + 3
                break
            case 7:
                program[program[i + 3]] = getValue(program, i + 1, param1Mode) < getValue(program, i + 2, param2Mode) ? 1 : 0
                i += 4
                break
            case 8:
                program[program[i + 3]] = getValue(program, i + 1, param1Mode) == getValue(program, i + 2, param2Mode) ? 1 : 0
                i += 4
                break
            case 99:
                return
            default:
                throw new Exception("Invalid opcode")
        }
    }
}

def inputFile = new File("input.txt")
def programStr = inputFile.text.trim()
def program = programStr.split(',').collect { it.toInteger() }

runProgram(program, 5)
