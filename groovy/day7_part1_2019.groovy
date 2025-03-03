
class IntcodeComputer {
    List<Long> memory
    int ip = 0
    Queue<Long> inputQueue = new LinkedList<>()
    Queue<Long> outputQueue = new LinkedList<>()
    boolean halted = false

    IntcodeComputer(List<Long> program) {
        this.memory = new ArrayList<>(program)
    }

    void provideInput(long input) {
        inputQueue.offer(input)
    }

    Long getOutput() {
        return outputQueue.poll()
    }
    
    boolean hasOutput() {
        return !outputQueue.isEmpty()
    }

    void run() {
        while (ip < memory.size()) {
            long opcode = memory[ip] % 100
            long mode1 = (memory[ip] / 100) % 10
            long mode2 = (memory[ip] / 1000) % 10
            long mode3 = (memory[ip] / 10000) % 10

            switch (opcode) {
                case 1: // Add
                    memory[getAddress(ip + 3, mode3)] = getValue(ip + 1, mode1) + getValue(ip + 2, mode2)
                    ip += 4
                    break
                case 2: // Multiply
                    memory[getAddress(ip + 3, mode3)] = getValue(ip + 1, mode1) * getValue(ip + 2, mode2)
                    ip += 4
                    break
                case 3: // Input
                    if (inputQueue.isEmpty()) {
                        return; // Wait for input
                    }
                    memory[getAddress(ip + 1, mode1)] = inputQueue.poll()
                    ip += 2
                    break
                case 4: // Output
                    outputQueue.offer(getValue(ip + 1, mode1))
                    ip += 2
                    break
                case 5: // Jump-if-true
                    if (getValue(ip + 1, mode1) != 0) {
                        ip = getValue(ip + 2, mode2).intValue()
                    } else {
                        ip += 3
                    }
                    break
                case 6: // Jump-if-false
                    if (getValue(ip + 1, mode1) == 0) {
                        ip = getValue(ip + 2, mode2).intValue()
                    } else {
                        ip += 3
                    }
                    break
                case 7: // Less than
                    memory[getAddress(ip + 3, mode3)] = (getValue(ip + 1, mode1) < getValue(ip + 2, mode2)) ? 1 : 0
                    ip += 4
                    break
                case 8: // Equals
                    memory[getAddress(ip + 3, mode3)] = (getValue(ip + 1, mode1) == getValue(ip + 2, mode2)) ? 1 : 0
                    ip += 4
                    break
                case 99: // Halt
                    halted = true;
                    return
                default:
                    throw new IllegalArgumentException("Invalid opcode: $opcode")
            }
        }
    }

    private long getValue(int address, long mode) {
        return mode == 0 ? memory[memory[address].intValue()] : memory[address]
    }

    private int getAddress(int address, long mode) {
        return mode == 0 ? memory[address].intValue() : address
    }
}


def runAmplifiers(List<Long> program, List<Integer> phaseSettings) {
    long inputSignal = 0
    for (int phaseSetting : phaseSettings) {
        IntcodeComputer computer = new IntcodeComputer(program)
        computer.provideInput(phaseSetting)
        computer.provideInput(inputSignal)
        computer.run()
        inputSignal = computer.getOutput()
    }
    return inputSignal
}

def findMaxThrusterSignal(List<Long> program) {
    long maxSignal = Long.MIN_VALUE
    [0, 1, 2, 3, 4].permutations().each { phaseSettings ->
        long signal = runAmplifiers(program, phaseSettings)
        maxSignal = Math.max(maxSignal, signal)
    }
    return maxSignal
}


def static main(args) {
    def inputFile = new File("input.txt")
    def program = inputFile.text.trim().split(',').collect { it.toLong() }

    def maxThrusterSignal = findMaxThrusterSignal(program)
    println maxThrusterSignal
}
