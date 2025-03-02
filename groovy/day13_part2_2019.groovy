
import java.nio.file.Files
import java.nio.file.Paths

class IntcodeComputer {

    List<Long> memory
    int instructionPointer = 0
    Queue<Long> inputQueue = new LinkedList<>()
    List<Long> output = new ArrayList<>()
    boolean halted = false
    long relativeBase = 0

    IntcodeComputer(List<Long> initialMemory) {
        this.memory = new ArrayList<>(initialMemory) // Create a copy
    }

    void addInput(long input) {
        inputQueue.add(input)
    }

    List<Long> run() {
        output.clear()
        while (!halted) {
            long instruction = memory[instructionPointer]
            long opcode = instruction % 100
            long mode1 = (instruction / 100) % 10
            long mode2 = (instruction / 1000) % 10
            long mode3 = (instruction / 10000) % 10

            switch (opcode) {
                case 1: { // Add
                    long param1 = getValue(instructionPointer + 1, mode1)
                    long param2 = getValue(instructionPointer + 2, mode2)
                    int address = getAddress(instructionPointer + 3, mode3)
                    memory[address] = param1 + param2
                    instructionPointer += 4
                    break
                }
                case 2: { // Multiply
                    long param1 = getValue(instructionPointer + 1, mode1)
                    long param2 = getValue(instructionPointer + 2, mode2)
                    int address = getAddress(instructionPointer + 3, mode3)
                    memory[address] = param1 * param2
                    instructionPointer += 4
                    break
                }
                case 3: { // Input
                    if (inputQueue.isEmpty()) {
                        return output // Pause execution if no input
                    }
                    int address = getAddress(instructionPointer + 1, mode1)
                    memory[address] = inputQueue.remove()
                    instructionPointer += 2
                    break
                }
                case 4: { // Output
                    long param1 = getValue(instructionPointer + 1, mode1)
                    output.add(param1)
                    instructionPointer += 2
                    break
                }
                case 5: { // Jump-if-true
                    long param1 = getValue(instructionPointer + 1, mode1)
                    long param2 = getValue(instructionPointer + 2, mode2)
                    if (param1 != 0) {
                        instructionPointer = param2.intValue()
                    } else {
                        instructionPointer += 3
                    }
                    break
                }
                case 6: { // Jump-if-false
                    long param1 = getValue(instructionPointer + 1, mode1)
                    long param2 = getValue(instructionPointer + 2, mode2)
                    if (param1 == 0) {
                        instructionPointer = param2.intValue()
                    } else {
                        instructionPointer += 3
                    }
                    break
                }
                case 7: { // Less than
                    long param1 = getValue(instructionPointer + 1, mode1)
                    long param2 = getValue(instructionPointer + 2, mode2)
                    int address = getAddress(instructionPointer + 3, mode3)
                    memory[address] = (param1 < param2) ? 1 : 0
                    instructionPointer += 4
                    break
                }
                case 8: { // Equals
                    long param1 = getValue(instructionPointer + 1, mode1)
                    long param2 = getValue(instructionPointer + 2, mode2)
                    int address = getAddress(instructionPointer + 3, mode3)
                    memory[address] = (param1 == param2) ? 1 : 0
                    instructionPointer += 4
                    break
                }
                case 9: { // Adjust relative base
                    long param1 = getValue(instructionPointer + 1, mode1)
                    relativeBase += param1
                    instructionPointer += 2
                    break
                }
                case 99: { // Halt
                    halted = true
                    return output
                }
                default:
                    throw new IllegalArgumentException("Unknown opcode: " + opcode)
            }
        }
        return output
    }


    long getValue(int parameterIndex, long mode) {
        long value = 0
        long rawValue = memory[parameterIndex]

        switch (mode) {
            case 0: // Position mode
                ensureCapacity(rawValue.intValue())
                value = memory[rawValue.intValue()]
                break
            case 1: // Immediate mode
                value = rawValue
                break
            case 2: // Relative mode
                ensureCapacity((relativeBase + rawValue).intValue())
                value = memory[(relativeBase + rawValue).intValue()]
                break
            default:
                throw new IllegalArgumentException("Unknown parameter mode: " + mode)
        }
        return value
    }

    int getAddress(int parameterIndex, long mode) {
        long rawValue = memory[parameterIndex]
        switch (mode) {
            case 0: // Position mode
                return rawValue.intValue()
            case 2: // Relative mode
                return (relativeBase + rawValue).intValue()
            default:
                throw new IllegalArgumentException("Invalid mode for address: " + mode)
        }
    }

    void ensureCapacity(int index) {
        while (memory.size() <= index) {
            memory.add(0L)
        }
    }
}


static void main(String[] args) {
    String filePath = "input.txt"
    List<Long> program = Files.readString(Paths.get(filePath))
            .trim()
            .split(",")
            .collect { it.toLong() }


    // Part 1
    IntcodeComputer computer1 = new IntcodeComputer(program)
    computer1.run()
    List<Long> output1 = computer1.output

    int blockTileCount = 0
    for (int i = 2; i < output1.size(); i += 3) {
        if (output1[i] == 2) {
            blockTileCount++
        }
    }
    println "Part 1: Number of block tiles: ${blockTileCount}"


    // Part 2
    List<Long> program2 = new ArrayList<>(program)  // Create a mutable copy

    while(program2.size() < 5000){
      program2.add(0L);
    }

    program2[0] = 2  // Insert two quarters


    IntcodeComputer computer2 = new IntcodeComputer(program2)
    long score = 0
    long ballX = 0
    long paddleX = 0

    while (!computer2.halted) {
        List<Long> output2 = computer2.run()

        for (int i = 0; i < output2.size(); i += 3) {
            long x = output2[i]
            long y = output2[i + 1]
            long tileId = output2[i + 2]

            if (x == -1 && y == 0) {
                score = tileId
            } else if (tileId == 3) {
                paddleX = x
            } else if (tileId == 4) {
                ballX = x
            }
        }


        if (!computer2.halted) {
            if (paddleX < ballX) {
                computer2.addInput(1)
            } else if (paddleX > ballX) {
                computer2.addInput(-1)
            } else {
                computer2.addInput(0)
            }
        }

    }
    println "Part 2: Final score: ${score}"
}
