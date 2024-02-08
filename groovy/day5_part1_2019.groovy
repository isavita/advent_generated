
def getMode(instruction, position) {
    return instruction / Math.pow(10, position+1) as int % 10
}

def getParam(program, pointer, mode) {
    if (mode == 0) {
        return program[program[pointer]]
    }
    return program[pointer]
}

def runProgram(program, input) {
    def output = 0
    def pointer = 0
    while (pointer < program.size()) {
        def instruction = program[pointer]
        def opcode = instruction % 100

        switch (opcode) {
            case 1:
            case 2:
                def param1 = getParam(program, pointer+1, getMode(instruction, 1))
                def param2 = getParam(program, pointer+2, getMode(instruction, 2))
                def result = 0
                if (opcode == 1) {
                    result = param1 + param2
                } else {
                    result = param1 * param2
                }
                program[program[pointer+3]] = result
                pointer += 4
                break
            case 3:
                program[program[pointer+1]] = input
                pointer += 2
                break
            case 4:
                output = getParam(program, pointer+1, getMode(instruction, 1))
                pointer += 2
                break
            case 99:
                return output
            default:
                throw new RuntimeException("Unknown opcode: $opcode")
        }
    }
    return output
}

def data = new File("input.txt").text.trim()
def strProgram = data.split(",")
def program = strProgram.collect { it as int }

println runProgram(program, 1)
