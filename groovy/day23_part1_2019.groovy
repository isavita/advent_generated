
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue

class IntcodeComputer {
    long[] memory
    long instructionPointer = 0
    long relativeBase = 0
    BlockingQueue<Long> inputQueue = new LinkedBlockingQueue<>()
    BlockingQueue<Long> outputQueue = new LinkedBlockingQueue<>()
    boolean halted = false

    IntcodeComputer(long[] program) {
        memory = program.clone()
    }

    void setInput(long input) {
        inputQueue.add(input)
    }

    long getOutput() {
        return outputQueue.take()
    }

    boolean isHalted() {
        return halted
    }

    long getMemory(long index) {
        if (index < 0) {
            throw new IllegalArgumentException("Negative index: " + index)
        }
        if (index >= memory.length) {
            memory = Arrays.copyOf(memory, (int) (index * 2)) // Auto-grow memory
        }
        return memory[(int) index]
    }

    void setMemory(long index, long value) {
        if (index < 0) {
            throw new IllegalArgumentException("Negative index: " + index)
        }
        if (index >= memory.length) {
            memory = Arrays.copyOf(memory, (int) (index * 2)) // Auto-grow memory
        }
        memory[(int) index] = value
    }

    long getParameter(int mode, int offset) {
        long parameter = getMemory(instructionPointer + offset)
        switch (mode) {
            case 0: // Position mode
                return getMemory(parameter)
            case 1: // Immediate mode
                return parameter
            case 2: // Relative mode
                return getMemory(relativeBase + parameter)
            default:
                throw new IllegalArgumentException("Unknown parameter mode: " + mode)
        }
    }

    void setParameter(int mode, int offset, long value) {
        long parameter = getMemory(instructionPointer + offset)
        switch (mode) {
            case 0: // Position mode
                setMemory(parameter, value)
                break
            case 2: // Relative mode
                setMemory(relativeBase + parameter, value)
                break
            default:
                throw new IllegalArgumentException("Invalid mode for writing: " + mode)
        }
    }


    void run() {
        while (!halted) {
            long instruction = getMemory(instructionPointer)
            int opcode = (int) (instruction % 100)
            int mode1 = (int) ((instruction / 100) % 10)
            int mode2 = (int) ((instruction / 1000) % 10)
            int mode3 = (int) ((instruction / 10000) % 10)

            switch (opcode) {
                case 1: // Add
                    long param1 = getParameter(mode1, 1)
                    long param2 = getParameter(mode2, 2)
                    setParameter(mode3, 3, param1 + param2)
                    instructionPointer += 4
                    break
                case 2: // Multiply
                    long param1 = getParameter(mode1, 1)
                    long param2 = getParameter(mode2, 2)
                    setParameter(mode3, 3, param1 * param2)
                    instructionPointer += 4
                    break
                case 3: // Input
                    if (inputQueue.isEmpty()) {
                        return // Wait for input
                    }
                    long inputValue = inputQueue.take()
                    setParameter(mode1, 1, inputValue)
                    instructionPointer += 2
                    break
                case 4: // Output
                    long outputValue = getParameter(mode1, 1)
                    outputQueue.add(outputValue)
                    instructionPointer += 2
                    break
                case 5: // Jump-if-true
                    long param1 = getParameter(mode1, 1)
                    long param2 = getParameter(mode2, 2)
                    if (param1 != 0) {
                        instructionPointer = param2
                    } else {
                        instructionPointer += 3
                    }
                    break
                case 6: // Jump-if-false
                    long param1 = getParameter(mode1, 1)
                    long param2 = getParameter(mode2, 2)
                    if (param1 == 0) {
                        instructionPointer = param2
                    } else {
                        instructionPointer += 3
                    }
                    break
                case 7: // Less than
                    long param1 = getParameter(mode1, 1)
                    long param2 = getParameter(mode2, 2)
                    setParameter(mode3, 3, (param1 < param2) ? 1 : 0)
                    instructionPointer += 4
                    break
                case 8: // Equals
                    long param1 = getParameter(mode1, 1)
                    long param2 = getParameter(mode2, 2)
                    setParameter(mode3, 3, (param1 == param2) ? 1 : 0)
                    instructionPointer += 4
                    break
                case 9: // Adjust relative base
                    long param1 = getParameter(mode1, 1)
                    relativeBase += param1
                    instructionPointer += 2
                    break
                case 99: // Halt
                    halted = true
                    return
                default:
                    throw new IllegalArgumentException("Unknown opcode: " + opcode + " at position " + instructionPointer)
            }
        }
    }
}

static void main(String[] args) {
    File file = new File("input.txt")
    String programString = file.text.trim()
    long[] program = programString.split(',').collect { it.toLong() }

    List<IntcodeComputer> computers = []
    List<BlockingQueue<List<Long>>> queues = []

    for (int i = 0; i < 50; i++) {
        IntcodeComputer computer = new IntcodeComputer(program)
        computer.setInput(i)
        computers.add(computer)
        queues.add(new LinkedBlockingQueue<>())
    }

    computers.eachWithIndex { computer, index ->
        Thread.start {
            while (!computer.isHalted()) {
                computer.run()
                while (computer.outputQueue.size() >= 3) {
                    long address = computer.getOutput()
                    long x = computer.getOutput()
                    long y = computer.getOutput()

                    if (address == 255) {
                        println y
                        System.exit(0)
                    } else if (address >= 0 && address < 50) {
                        queues[address.intValue()].add([x, y])
                    } else {
                        println "Invalid address: $address"
                    }
                }

                if (queues[index].isEmpty()) {
                    computer.setInput(-1)
                } else {
                    List<Long> packet = queues[index].take()
                    computer.setInput(packet[0])
                    computer.setInput(packet[1])
                }
            }
        }
    }
}
