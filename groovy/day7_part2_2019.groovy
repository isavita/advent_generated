
class IntcodeComputer {
    List<Long> memory
    int pointer = 0
    List<Long> inputQueue = []
    Long lastOutput = 0
    boolean halted = false
    boolean waitingForInput = false

    IntcodeComputer(List<Long> program) {
        this.memory = new ArrayList<>(program)
    }

    void addInput(Long input) {
        inputQueue.add(input)
        waitingForInput = false;
    }

    Long run() {
        while (pointer < memory.size()) {
            long opcode = memory[pointer] % 100
            long mode1 = (memory[pointer] / 100) % 10
            long mode2 = (memory[pointer] / 1000) % 10
            long mode3 = (memory[pointer] / 10000) % 10

            switch (opcode) {
                case 1: // Add
                    setParameter(3, mode3, getParameter(1, mode1) + getParameter(2, mode2))
                    pointer += 4
                    break
                case 2: // Multiply
                    setParameter(3, mode3, getParameter(1, mode1) * getParameter(2, mode2))
                    pointer += 4
                    break
                case 3: // Input
                    if (inputQueue.isEmpty()) {
                        waitingForInput = true;
                        return null // Pause execution, waiting for input
                    }
                    setParameter(1, mode1, inputQueue.remove(0))
                    pointer += 2
                    break
                case 4: // Output
                    lastOutput = getParameter(1, mode1)
                    pointer += 2
                    return lastOutput
                case 5: // Jump-if-true
                    if (getParameter(1, mode1) != 0) {
                        pointer = getParameter(2, mode2).intValue()
                    } else {
                        pointer += 3
                    }
                    break
                case 6: // Jump-if-false
                    if (getParameter(1, mode1) == 0) {
                        pointer = getParameter(2, mode2).intValue()
                    } else {
                        pointer += 3
                    }
                    break
                case 7: // Less than
                    setParameter(3, mode3, getParameter(1, mode1) < getParameter(2, mode2) ? 1 : 0)
                    pointer += 4
                    break
                case 8: // Equals
                    setParameter(3, mode3, getParameter(1, mode1) == getParameter(2, mode2) ? 1 : 0)
                    pointer += 4
                    break
                case 99: // Halt
                    halted = true
                    return null
                default:
                    throw new IllegalArgumentException("Unknown opcode: $opcode")
            }
        }
        return null; //should not reach here
    }

    private Long getParameter(int offset, long mode) {
        switch (mode) {
            case 0: // Position mode
                return memory[memory[pointer + offset].intValue()]
            case 1: // Immediate mode
                return memory[pointer + offset]
            default:
                throw new IllegalArgumentException("Unknown parameter mode: $mode")
        }
    }

    private void setParameter(int offset, long mode, Long value) {
        switch (mode) {
            case 0: // Position mode
                memory[memory[pointer + offset].intValue()] = value
                break
            default:
                throw new IllegalArgumentException("Unknown parameter mode: $mode")
        }
    }
}


def runAmplifiers(List<Long> program, List<Integer> phaseSettings) {
    Long signal = 0
    phaseSettings.each { phase ->
        IntcodeComputer computer = new IntcodeComputer(program)
        computer.addInput(phase.toLong())
        computer.addInput(signal)
        signal = computer.run()
    }
    return signal
}

def runAmplifiersFeedbackLoop(List<Long> program, List<Integer> phaseSettings) {
    def amplifiers = phaseSettings.collect { phase ->
        def computer = new IntcodeComputer(program)
        computer.addInput(phase.toLong())
        computer
    }

    Long signal = 0
    int currentAmp = 0
    while (!amplifiers.every { it.halted }) {
        amplifiers[currentAmp].addInput(signal)
        def output = amplifiers[currentAmp].run()
        if (output != null) {
            signal = output
        }
        currentAmp = (currentAmp + 1) % amplifiers.size()
    }

    return signal  // Return the last output from Amp E (which is the final 'signal')
}

def findMaxThrusterSignal(List<Long> program, boolean feedbackLoop = false) {
    def phaseSettingsRange = feedbackLoop ? (5..9) : (0..4)
    def maxSignal = Long.MIN_VALUE

    phaseSettingsRange.toList().permutations().each { permutation ->
        Long signal = feedbackLoop ? runAmplifiersFeedbackLoop(program, permutation) : runAmplifiers(program, permutation)

        if (signal != null)
        {
            maxSignal = Math.max(maxSignal, signal)
        }

    }
    return maxSignal
}


def static main(args) {
    def inputFile = new File("input.txt")
    def program = inputFile.text.trim().split(',').collect { it.toLong() }

    // Part 1
    def maxSignal = findMaxThrusterSignal(program)
    println "Part 1 - Max Thruster Signal: $maxSignal"

    // Part 2
    def maxSignalFeedback = findMaxThrusterSignal(program, true)
    println "Part 2 - Max Thruster Signal (Feedback Loop): $maxSignalFeedback"
}
