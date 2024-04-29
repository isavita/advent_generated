class IntcodeComputer {
    List<Long> memory
    Integer pointer
    Integer relativeBase
    List<Long> input
    Boolean testMode

    IntcodeComputer(List<Long> program, List<Long> input, Boolean testMode) {
        this.memory = new ArrayList<>(program)
        this.memory.addAll(Collections.nCopies(10000, 0L)) // initialize extra memory
        this.pointer = 0
        this.relativeBase = 0
        this.input = input
        this.testMode = testMode
    }

    void run() {
        while (true) {
            if (pointer >= memory.size()) {
                break
            }
            Long opcode = memory[pointer]
            Long modes = opcode / 100
            Long instruction = opcode % 100

            List<Long> parameters = []
            for (int i = 0; i < getParameterCount(instruction); i++) {
                parameters.add(memory[pointer + i + 1])
            }

            switch (instruction) {
                case 1: // add
                    add(parameters, modes)
                    break
                case 2: // multiply
                    multiply(parameters, modes)
                    break
                case 3: // input
                    input(parameters, modes)
                    break
                case 4: // output
                    output(parameters, modes)
                    break
                case 5: // jump-if-true
                    jumpIfTrue(parameters, modes)
                    break
                case 6: // jump-if-false
                    jumpIfFalse(parameters, modes)
                    break
                case 7: // less than
                    lessThan(parameters, modes)
                    break
                case 8: // equals
                    equals(parameters, modes)
                    break
                case 9: // adjust relative base
                    adjustRelativeBase(parameters, modes)
                    break
                case 99: // halt
                    return
                default:
                    throw new RuntimeException("Invalid opcode: $opcode")
            }
        }
    }

    private void add(List<Long> parameters, Long modes) {
        Long value1 = getParameter(parameters, modes, 0)
        Long value2 = getParameter(parameters, modes, 1)
        Long result = value1 + value2
        setParameter(parameters, modes, 2, result)
        pointer += 4
    }

    private void multiply(List<Long> parameters, Long modes) {
        Long value1 = getParameter(parameters, modes, 0)
        Long value2 = getParameter(parameters, modes, 1)
        Long result = value1 * value2
        setParameter(parameters, modes, 2, result)
        pointer += 4
    }

    private void input(List<Long> parameters, Long modes) {
        Long value = input.remove(0)
        setParameter(parameters, modes, 0, value)
        pointer += 2
    }

    private void output(List<Long> parameters, Long modes) {
        Long value = getParameter(parameters, modes, 0)
        if (testMode) {
            println(value)
        } else {
            print(value + " ")
        }
        pointer += 2
    }

    private void jumpIfTrue(List<Long> parameters, Long modes) {
        Long value = getParameter(parameters, modes, 0)
        if (value != 0) {
            pointer = getParameter(parameters, modes, 1) as Integer
        } else {
            pointer += 3
        }
    }

    private void jumpIfFalse(List<Long> parameters, Long modes) {
        Long value = getParameter(parameters, modes, 0)
        if (value == 0) {
            pointer = getParameter(parameters, modes, 1) as Integer
        } else {
            pointer += 3
        }
    }

    private void lessThan(List<Long> parameters, Long modes) {
        Long value1 = getParameter(parameters, modes, 0)
        Long value2 = getParameter(parameters, modes, 1)
        Long result = value1 < value2 ? 1 : 0
        setParameter(parameters, modes, 2, result)
        pointer += 4
    }

    private void equals(List<Long> parameters, Long modes) {
        Long value1 = getParameter(parameters, modes, 0)
        Long value2 = getParameter(parameters, modes, 1)
        Long result = value1 == value2 ? 1 : 0
        setParameter(parameters, modes, 2, result)
        pointer += 4
    }

    private void adjustRelativeBase(List<Long> parameters, Long modes) {
        Long value = getParameter(parameters, modes, 0)
        relativeBase += value
        pointer += 2
    }

    private Long getParameter(List<Long> parameters, Long modes, Integer index) {
        Long mode = (modes / Math.pow(10, index)) % 10
        switch (mode) {
            case 0: // position mode
                return memory[memory[pointer + index + 1] as Integer]
            case 1: // immediate mode
                return parameters[index]
            case 2: // relative mode
                return memory[(memory[pointer + index + 1] + relativeBase) as Integer]
            default:
                throw new RuntimeException("Invalid mode: $mode")
        }
    }

    private void setParameter(List<Long> parameters, Long modes, Integer index, Long value) {
        Long mode = (modes / Math.pow(10, index)) % 10
        switch (mode) {
            case 0: // position mode
                memory[memory[pointer + index + 1] as Integer] = value
                break
            case 2: // relative mode
                memory[(memory[pointer + index + 1] + relativeBase) as Integer] = value
                break
            default:
                throw new RuntimeException("Invalid mode: $mode")
        }
    }

    private Integer getParameterCount(Long instruction) {
        switch (instruction) {
            case 1: // add
            case 2: // multiply
            case 7: // less than
            case 8: // equals
                return 3
            case 3: // input
            case 4: // output
                return 1
            case 5: // jump-if-true
            case 6: // jump-if-false
                return 2
            case 9: // adjust relative base
                return 1
            case 99: // halt
                return 0
            default:
                throw new RuntimeException("Invalid opcode: $instruction")
        }
    }
}

def input = new File('input.txt').readLines()[0].split(',').collect { it.toLong() }
def computer = new IntcodeComputer(input, [1], true) // run in test mode
computer.run()

computer = new IntcodeComputer(input, [2], false) // run in sensor boost mode
computer.run()