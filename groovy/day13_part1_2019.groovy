
class IntcodeComputer {
    private List<Long> memory
    private int instructionPointer = 0
    private int relativeBase = 0

    IntcodeComputer(List<Long> memory) {
        this.memory = memory
    }

    private long readMemory(int address) {
        if (address >= memory.size()) {
            memory.addAll(Collections.nCopies(address - memory.size() + 1, 0L))
        }
        return memory[address]
    }

    private void writeMemory(int address, long value) {
        if (address >= memory.size()) {
            memory.addAll(Collections.nCopies(address - memory.size() + 1, 0L))
        }
        memory[address] = value
    }

    private int getParameterAddress(int parameterIndex, int mode) {
        switch (mode) {
            case 0: return readMemory(instructionPointer + parameterIndex) as int
            case 1: return instructionPointer + parameterIndex
            case 2: return relativeBase + readMemory(instructionPointer + parameterIndex) as int
            default: throw new IllegalArgumentException("Unknown parameter mode: $mode")
        }
    }

    private long getParameterValue(int parameterIndex, int mode) {
        return readMemory(getParameterAddress(parameterIndex, mode))
    }

    List<Long> run() {
        List<Long> outputs = []
        while (true) {
            long instruction = readMemory(instructionPointer)
            int opcode = instruction % 100
            int mode1 = (instruction / 100) % 10
            int mode2 = (instruction / 1000) % 10
            int mode3 = (instruction / 10000) % 10

            switch (opcode) {
                case 1:
                    long param1 = getParameterValue(1, mode1)
                    long param2 = getParameterValue(2, mode2)
                    int address3 = getParameterAddress(3, mode3)
                    writeMemory(address3, param1 + param2)
                    instructionPointer += 4
                    break
                case 2:
                    long param1 = getParameterValue(1, mode1)
                    long param2 = getParameterValue(2, mode2)
                    int address3 = getParameterAddress(3, mode3)
                    writeMemory(address3, param1 * param2)
                    instructionPointer += 4
                    break
                case 3:
                    int address1 = getParameterAddress(1, mode1)
                    // For this problem, we don't need input, so we can just use 0
                    writeMemory(address1, 0)
                    instructionPointer += 2
                    break
                case 4:
                    long param1 = getParameterValue(1, mode1)
                    outputs.add(param1)
                    instructionPointer += 2
                    break
                case 5:
                    long param1 = getParameterValue(1, mode1)
                    long param2 = getParameterValue(2, mode2)
                    if (param1 != 0) {
                        instructionPointer = param2 as int
                    } else {
                        instructionPointer += 3
                    }
                    break
                case 6:
                    long param1 = getParameterValue(1, mode1)
                    long param2 = getParameterValue(2, mode2)
                    if (param1 == 0) {
                        instructionPointer = param2 as int
                    } else {
                        instructionPointer += 3
                    }
                    break
                case 7:
                    long param1 = getParameterValue(1, mode1)
                    long param2 = getParameterValue(2, mode2)
                    int address3 = getParameterAddress(3, mode3)
                    writeMemory(address3, param1 < param2 ? 1 : 0)
                    instructionPointer += 4
                    break
                case 8:
                    long param1 = getParameterValue(1, mode1)
                    long param2 = getParameterValue(2, mode2)
                    int address3 = getParameterAddress(3, mode3)
                    writeMemory(address3, param1 == param2 ? 1 : 0)
                    instructionPointer += 4
                    break
                case 9:
                    long param1 = getParameterValue(1, mode1)
                    relativeBase += param1 as int
                    instructionPointer += 2
                    break
                case 99:
                    return outputs
                default:
                    throw new IllegalArgumentException("Unknown opcode: $opcode")
            }
        }
    }
}

def readInputFile(String filename) {
    new File(filename).text.split(',').collect { it.toLong() }
}

def countBlockTiles(List<Long> outputs) {
    int blockTileCount = 0
    for (int i = 2; i < outputs.size(); i += 3) {
        if (outputs[i] == 2) {
            blockTileCount++
        }
    }
    return blockTileCount
}

def main() {
    List<Long> memory = readInputFile('input.txt')
    IntcodeComputer computer = new IntcodeComputer(memory)
    List<Long> outputs = computer.run()
    int blockTileCount = countBlockTiles(outputs)
    println "Number of block tiles: $blockTileCount"
}

main()
