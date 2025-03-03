
import java.awt.Point

enum Direction {
    UP, RIGHT, DOWN, LEFT
}

class IntcodeComputer {
    private List<BigInteger> memory
    private BigInteger instructionPointer = 0
    private BigInteger relativeBase = 0
    private Queue<BigInteger> inputQueue = new LinkedList<>()
    private List<BigInteger> outputList = new LinkedList<>()
    boolean halted = false

    IntcodeComputer(List<BigInteger> program) {
        this.memory = new ArrayList<>(program)
    }

    void addInput(BigInteger input) {
        inputQueue.add(input)
    }

    List<BigInteger> getOutput() {
        return outputList
    }

    void clearOutput() {
        outputList.clear()
    }

    BigInteger getMemoryValue(BigInteger index) {
        while (memory.size() <= index.intValue()) {
            memory.add(BigInteger.ZERO)
        }
        return memory.get(index.intValue())
    }

    void setMemoryValue(BigInteger index, BigInteger value) {
        while (memory.size() <= index.intValue()) {
            memory.add(BigInteger.ZERO)
        }
        memory.set(index.intValue(), value)
    }

    BigInteger getParameterValue(int mode, BigInteger parameter) {
        if (mode == 0) { // Position mode
            return getMemoryValue(parameter)
        } else if (mode == 1) { // Immediate mode
            return parameter
        } else if (mode == 2) { // Relative mode
            return getMemoryValue(relativeBase.add(parameter))
        } else {
            throw new IllegalArgumentException("Invalid parameter mode: " + mode)
        }
    }

    void setParameterValue(int mode, BigInteger parameter, BigInteger value) {
        if (mode == 0) { // Position mode
            setMemoryValue(parameter, value)
        } else if (mode == 2) { // Relative mode
            setMemoryValue(relativeBase.add(parameter), value)
        } else {
            throw new IllegalArgumentException("Invalid parameter mode for write: " + mode)
        }
    }

    void run() {
        while (!halted) {
            BigInteger instruction = getMemoryValue(instructionPointer)
            int opcode = instruction.mod(BigInteger.valueOf(100)).intValue()
            int mode1 = instruction.divide(BigInteger.valueOf(100)).mod(BigInteger.TEN).intValue()
            int mode2 = instruction.divide(BigInteger.valueOf(1000)).mod(BigInteger.TEN).intValue()
            int mode3 = instruction.divide(BigInteger.valueOf(10000)).mod(BigInteger.TEN).intValue()

            switch (opcode) {
                case 1: { // Add
                    BigInteger param1 = getMemoryValue(instructionPointer.add(BigInteger.ONE))
                    BigInteger param2 = getMemoryValue(instructionPointer.add(BigInteger.valueOf(2)))
                    BigInteger param3 = getMemoryValue(instructionPointer.add(BigInteger.valueOf(3)))

                    BigInteger value1 = getParameterValue(mode1, param1)
                    BigInteger value2 = getParameterValue(mode2, param2)

                    setParameterValue(mode3, param3, value1.add(value2))
                    instructionPointer = instructionPointer.add(BigInteger.valueOf(4))
                    break
                }
                case 2: { // Multiply
                    BigInteger param1 = getMemoryValue(instructionPointer.add(BigInteger.ONE))
                    BigInteger param2 = getMemoryValue(instructionPointer.add(BigInteger.valueOf(2)))
                    BigInteger param3 = getMemoryValue(instructionPointer.add(BigInteger.valueOf(3)))

                    BigInteger value1 = getParameterValue(mode1, param1)
                    BigInteger value2 = getParameterValue(mode2, param2)

                    setParameterValue(mode3, param3, value1.multiply(value2))
                    instructionPointer = instructionPointer.add(BigInteger.valueOf(4))
                    break
                }
                case 3: { // Input
                    if (inputQueue.isEmpty()) {
                        return // Wait for input
                    }
                    BigInteger param1 = getMemoryValue(instructionPointer.add(BigInteger.ONE))
                    BigInteger inputValue = inputQueue.poll()
                    setParameterValue(mode1, param1, inputValue)
                    instructionPointer = instructionPointer.add(BigInteger.valueOf(2))
                    break
                }
                case 4: { // Output
                    BigInteger param1 = getMemoryValue(instructionPointer.add(BigInteger.ONE))
                    BigInteger outputValue = getParameterValue(mode1, param1)
                    outputList.add(outputValue)
                    instructionPointer = instructionPointer.add(BigInteger.valueOf(2))
                    break
                }
                case 5: { // Jump-if-true
                    BigInteger param1 = getMemoryValue(instructionPointer.add(BigInteger.ONE))
                    BigInteger param2 = getMemoryValue(instructionPointer.add(BigInteger.valueOf(2)))

                    BigInteger value1 = getParameterValue(mode1, param1)
                    BigInteger value2 = getParameterValue(mode2, param2)

                    if (!value1.equals(BigInteger.ZERO)) {
                        instructionPointer = value2
                    } else {
                        instructionPointer = instructionPointer.add(BigInteger.valueOf(3))
                    }
                    break
                }
                case 6: { // Jump-if-false
                    BigInteger param1 = getMemoryValue(instructionPointer.add(BigInteger.ONE))
                    BigInteger param2 = getMemoryValue(instructionPointer.add(BigInteger.valueOf(2)))

                    BigInteger value1 = getParameterValue(mode1, param1)
                    BigInteger value2 = getParameterValue(mode2, param2)

                    if (value1.equals(BigInteger.ZERO)) {
                        instructionPointer = value2
                    } else {
                        instructionPointer = instructionPointer.add(BigInteger.valueOf(3))
                    }
                    break
                }
                case 7: { // Less than
                    BigInteger param1 = getMemoryValue(instructionPointer.add(BigInteger.ONE))
                    BigInteger param2 = getMemoryValue(instructionPointer.add(BigInteger.valueOf(2)))
                    BigInteger param3 = getMemoryValue(instructionPointer.add(BigInteger.valueOf(3)))

                    BigInteger value1 = getParameterValue(mode1, param1)
                    BigInteger value2 = getParameterValue(mode2, param2)

                    if (value1.compareTo(value2) < 0) {
                        setParameterValue(mode3, param3, BigInteger.ONE)
                    } else {
                        setParameterValue(mode3, param3, BigInteger.ZERO)
                    }
                    instructionPointer = instructionPointer.add(BigInteger.valueOf(4))
                    break
                }
                case 8: { // Equals
                    BigInteger param1 = getMemoryValue(instructionPointer.add(BigInteger.ONE))
                    BigInteger param2 = getMemoryValue(instructionPointer.add(BigInteger.valueOf(2)))
                    BigInteger param3 = getMemoryValue(instructionPointer.add(BigInteger.valueOf(3)))

                    BigInteger value1 = getParameterValue(mode1, param1)
                    BigInteger value2 = getParameterValue(mode2, param2)

                    if (value1.equals(value2)) {
                        setParameterValue(mode3, param3, BigInteger.ONE)
                    } else {
                        setParameterValue(mode3, param3, BigInteger.ZERO)
                    }
                    instructionPointer = instructionPointer.add(BigInteger.valueOf(4))
                    break
                }
                case 9: { // Adjust relative base
                    BigInteger param1 = getMemoryValue(instructionPointer.add(BigInteger.ONE))
                    BigInteger value1 = getParameterValue(mode1, param1)
                    relativeBase = relativeBase.add(value1)
                    instructionPointer = instructionPointer.add(BigInteger.valueOf(2))
                    break
                }
                case 99: { // Halt
                    halted = true
                    return
                }
                default:
                    throw new IllegalArgumentException("Invalid opcode: " + opcode)
            }
        }
    }
}

static void main(String[] args) {
    File file = new File("input.txt")
    List<BigInteger> program = file.text.trim().split(',').collect { new BigInteger(it) }

    // Part 1
    Map<Point, Integer> panels = [:]
    Point currentPosition = new Point(0, 0)
    Direction currentDirection = Direction.UP

    IntcodeComputer computer = new IntcodeComputer(program)

    while (!computer.halted) {
        int panelColor = panels.getOrDefault(currentPosition, 0)
        computer.addInput(BigInteger.valueOf(panelColor))
        computer.run()

        List<BigInteger> output = computer.getOutput()
        if (output.size() >= 2) {
            int color = output[0].intValue()
            int turn = output[1].intValue()
            computer.clearOutput()

            panels.put(new Point(currentPosition), color)

            if (turn == 0) { // Turn left
                switch (currentDirection) {
                    case Direction.UP: currentDirection = Direction.LEFT; break
                    case Direction.RIGHT: currentDirection = Direction.UP; break
                    case Direction.DOWN: currentDirection = Direction.RIGHT; break
                    case Direction.LEFT: currentDirection = Direction.DOWN; break
                }
            } else { // Turn right
                switch (currentDirection) {
                    case Direction.UP: currentDirection = Direction.RIGHT; break
                    case Direction.RIGHT: currentDirection = Direction.DOWN; break
                    case Direction.DOWN: currentDirection = Direction.LEFT; break
                    case Direction.LEFT: currentDirection = Direction.UP; break
                }
            }

            switch (currentDirection) {
                case Direction.UP: currentPosition.y--; break
                case Direction.RIGHT: currentPosition.x++; break
                case Direction.DOWN: currentPosition.y++; break
                case Direction.LEFT: currentPosition.x--; break
            }
        } else if (!computer.halted) {
            Thread.sleep(10) // avoid busy-waiting
        }

    }

    println "Part 1: Number of panels painted at least once: ${panels.size()}"

    // Part 2
    panels = [:]
    currentPosition = new Point(0, 0)
    currentDirection = Direction.UP
    panels.put(new Point(0, 0), 1) // Start on a white panel

    computer = new IntcodeComputer(program)

    while (!computer.halted) {
        int panelColor = panels.getOrDefault(currentPosition, 0)
        computer.addInput(BigInteger.valueOf(panelColor))
        computer.run()

        List<BigInteger> output = computer.getOutput()
        if (output.size() >= 2) {
            int color = output[0].intValue()
            int turn = output[1].intValue()
            computer.clearOutput()

            panels.put(new Point(currentPosition), color)

            if (turn == 0) { // Turn left
                switch (currentDirection) {
                    case Direction.UP: currentDirection = Direction.LEFT; break
                    case Direction.RIGHT: currentDirection = Direction.UP; break
                    case Direction.DOWN: currentDirection = Direction.RIGHT; break
                    case Direction.LEFT: currentDirection = Direction.DOWN; break
                }
            } else { // Turn right
                switch (currentDirection) {
                    case Direction.UP: currentDirection = Direction.RIGHT; break
                    case Direction.RIGHT: currentDirection = Direction.DOWN; break
                    case Direction.DOWN: currentDirection = Direction.LEFT; break
                    case Direction.LEFT: currentDirection = Direction.UP; break
                }
            }

            switch (currentDirection) {
                case Direction.UP: currentPosition.y--; break
                case Direction.RIGHT: currentPosition.x++; break
                case Direction.DOWN: currentPosition.y++; break
                case Direction.LEFT: currentPosition.x--; break
            }
        }
    }

    // Find boundaries for printing
    int minX = panels.keySet().collect { it.x }.min()
    int maxX = panels.keySet().collect { it.x }.max()
    int minY = panels.keySet().collect { it.y }.min()
    int maxY = panels.keySet().collect { it.y }.max()

    println "Part 2: Registration Identifier:"
    for (int y = minY; y <= maxY; y++) {
        for (int x = minX; x <= maxX; x++) {
            int color = panels.getOrDefault(new Point(x, y), 0)
            print color == 0 ? ' ' : '#'
        }
        println()
    }
}
