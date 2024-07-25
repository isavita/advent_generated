
class IntcodeComputer {
    List<Long> memory
    Long relativeBase = 0
    int instructionPointer = 0

    IntcodeComputer(List<Long> program) {
        memory = new ArrayList<>(Collections.nCopies(10000, 0L)) // Initialize memory with extra space
        for (int i = 0; i < program.size(); i++) {
            memory[i] = program[i]
        }
    }

    Long getValue(Long address) {
        return address >= 0 ? memory[address.intValue()] : 0
    }

    void setValue(Long address, Long value) {
        if (address >= 0) {
            memory[address.intValue()] = value
        }
    }

    Long getParameter(int mode, Long param) {
        switch (mode) {
            case 0: // Position mode
                return getValue(param)
            case 1: // Immediate mode
                return param
            case 2: // Relative mode
                return getValue(relativeBase + param)
            default:
                throw new IllegalArgumentException("Unknown mode: " + mode)
        }
    }

    void setParameter(int mode, Long param, Long value) {
        switch (mode) {
            case 0: // Position mode
                setValue(param, value)
                break
            case 2: // Relative mode
                setValue(relativeBase + param, value)
                break
            default:
                throw new IllegalArgumentException("Unknown mode: " + mode)
        }
    }

    void run(Long input) {
        while (true) {
            Long instruction = getValue(instructionPointer)
            int opcode = (int) (instruction % 100)
            int mode1 = (int) (instruction / 100) % 10
            int mode2 = (int) (instruction / 1000) % 10
            int mode3 = (int) (instruction / 10000) % 10

            switch (opcode) {
                case 1: // Add
                    setParameter(mode3, getValue(instructionPointer + 3), getParameter(mode1, getValue(instructionPointer + 1)) + getParameter(mode2, getValue(instructionPointer + 2)))
                    instructionPointer += 4
                    break
                case 2: // Multiply
                    setParameter(mode3, getValue(instructionPointer + 3), getParameter(mode1, getValue(instructionPointer + 1)) * getParameter(mode2, getValue(instructionPointer + 2)))
                    instructionPointer += 4
                    break
                case 3: // Input
                    setParameter(mode1, getValue(instructionPointer + 1), input)
                    instructionPointer += 2
                    break
                case 4: // Output
                    Long output = getParameter(mode1, getValue(instructionPointer + 1))
                    println output // Print the output
                    instructionPointer += 2
                    break
                case 5: // Jump-if-true
                    instructionPointer = getParameter(mode1, getValue(instructionPointer + 1)) != 0 ? getParameter(mode2, getValue(instructionPointer + 2)).intValue() : instructionPointer + 3
                    break
                case 6: // Jump-if-false
                    instructionPointer = getParameter(mode1, getValue(instructionPointer + 1)) == 0 ? getParameter(mode2, getValue(instructionPointer + 2)).intValue() : instructionPointer + 3
                    break
                case 7: // Less than
                    setParameter(mode3, getValue(instructionPointer + 3), getParameter(mode1, getValue(instructionPointer + 1)) < getParameter(mode2, getValue(instructionPointer + 2)) ? 1 : 0)
                    instructionPointer += 4
                    break
                case 8: // Equals
                    setParameter(mode3, getValue(instructionPointer + 3), getParameter(mode1, getValue(instructionPointer + 1)) == getParameter(mode2, getValue(instructionPointer + 2)) ? 1 : 0)
                    instructionPointer += 4
                    break
                case 9: // Adjust relative base
                    relativeBase += getParameter(mode1, getValue(instructionPointer + 1))
                    instructionPointer += 2
                    break
                case 99: // Halt
                    return
                default:
                    throw new IllegalArgumentException("Unknown opcode: " + opcode)
            }
        }
    }
}

def readProgramFromFile(String filename) {
    new File(filename).text.trim().split(',').collect { it.toLong() }
}

def program = readProgramFromFile('input.txt')
def computer = new IntcodeComputer(program)
computer.run(1) // Run the program with input 1
