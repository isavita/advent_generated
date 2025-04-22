
import java.nio.file.Files
import java.nio.file.Paths

/**
 * Intcode Computer implementation for Advent of Code 2019.
 * Handles opcodes 1-8, 99, parameter modes, and relative base.
 */
class IntcodeComputer {
    // Memory using a map for sparse access and automatic 0 default
    Map<Long, Long> memory = new HashMap<Long, Long>().withDefault { 0L }
    long ip = 0 // Instruction pointer
    long rb = 0 // Relative base

    // Input/Output queues
    List<Long> inputs = []
    List<Long> outputs = []

    /**
     * Constructor to load the initial program.
     * @param program The list of Long values representing the program.
     */
    IntcodeComputer(List<Long> program) {
        program.eachWithIndex { val, idx -> memory[(long)idx] = val }
    }

    /**
     * Helper to get the value of a parameter based on its mode.
     * @param mode The parameter mode (0: Position, 1: Immediate, 2: Relative).
     * @param offset The offset from the instruction pointer to the parameter's value/address.
     * @return The actual value to be used in the operation.
     */
    private long readParam(int mode, int offset) {
        long address = ip + offset
        long paramValue = memory[address] // This is the value *at* ip+offset

        switch (mode) {
            case 0: // Position mode
                return memory[paramValue] // Value *at* the address given by paramValue
            case 1: // Immediate mode
                return paramValue // The value itself
            case 2: // Relative mode
                return memory[rb + paramValue] // Value *at* address relative base + paramValue
            default:
                throw new IllegalArgumentException("Unknown mode: $mode")
        }
    }

    /**
     * Helper to get the memory address where a result should be written.
     * @param mode The parameter mode (0: Position, 2: Relative). Mode 1 is invalid for writes.
     * @param offset The offset from the instruction pointer to the parameter's address.
     * @return The memory address to write to.
     */
    private long writeAddress(int mode, int offset) {
        long address = ip + offset
        long paramValue = memory[address] // This is the address/offset value

        switch (mode) {
            case 0: // Position mode
                return paramValue
            case 2: // Relative mode
                return rb + paramValue
            default:
                throw new IllegalArgumentException("Unknown write mode: $mode") // Mode 1 is invalid for writing
        }
    }

    /**
     * Runs the Intcode program until it halts or requests input.
     * @return True if the program halted (opcode 99), false if it requires input (opcode 3).
     */
    boolean run() {
        while (true) {
            long instruction = memory[ip]
            int opcode = (int) (instruction % 100)
            int mode1 = (int) ((instruction / 100) % 10)
            int mode2 = (int) ((instruction / 1000) % 10)
            int mode3 = (int) ((instruction / 10000) % 10) // Mode for write parameters

            switch (opcode) {
                case 1: // Add
                    long val1 = readParam(mode1, 1)
                    long val2 = readParam(mode2, 2)
                    long addr3 = writeAddress(mode3, 3)
                    memory[addr3] = val1 + val2
                    ip += 4
                    break
                case 2: // Multiply
                    long valA = readParam(mode1, 1)
                    long valB = readParam(mode2, 2)
                    long addrC = writeAddress(mode3, 3)
                    memory[addrC] = valA * valB
                    ip += 4
                    break
                case 3: // Input
                    // For Part 1, the program only outputs and doesn't need input.
                    // If it ever hits opcode 3, something is wrong for this specific problem part.
                     throw new IllegalStateException("Input opcode encountered, but no input is expected for Part 1")
                    // Standard Intcode input handling if needed:
                    // if (inputs.isEmpty()) return false // Indicate need for input
                    // long inputVal = inputs.remove(0)
                    // long writeAddr = writeAddress(mode1, 1)
                    // memory[writeAddr] = inputVal
                    // ip += 2
                    // break
                case 4: // Output
                    long outputVal = readParam(mode1, 1)
                    outputs << outputVal
                    ip += 2
                    break
                case 5: // Jump if true
                    long testVal = readParam(mode1, 1)
                    long jumpAddr = readParam(mode2, 2)
                    if (testVal != 0) {
                        ip = jumpAddr
                    } else {
                        ip += 3
                    }
                    break
                case 6: // Jump if false
                    long testValFalse = readParam(mode1, 1)
                    long jumpAddrFalse = readParam(mode2, 2)
                    if (testValFalse == 0) {
                        ip = jumpAddrFalse
                    } else {
                        ip += 3
                    }
                    break
                case 7: // Less than
                    long valX = readParam(mode1, 1)
                    long valY = readParam(mode2, 2)
                    long addrZ = writeAddress(mode3, 3)
                    memory[addrZ] = (valX < valY) ? 1L : 0L
                    ip += 4
                    break
                case 8: // Equals
                    long valP = readParam(mode1, 1)
                    long valQ = readParam(mode2, 2)
                    long addrR = writeAddress(mode3, 3)
                    memory[addrR] = (valP == valQ) ? 1L : 0L
                    ip += 4
                    break
                case 9: // Adjust relative base
                    long offset = readParam(mode1, 1)
                    rb += offset
                    ip += 2
                    break
                case 99: // Halt
                    return true // Indicate program halted
                default:
                    throw new IllegalStateException("Unknown opcode: $opcode at position $ip")
            }
        }
    }
}

/**
 * Main entry point for the Groovy script.
 * Reads Intcode program, runs it, processes output into a grid,
 * finds intersections, calculates alignment parameters, and prints the sum.
 * @param args Command line arguments (optional: first argument can be input file path).
 */
static void main(String[] args) {
    // Determine input file path
    String filePath = args.length > 0 ? args[0] : 'input.txt'
    File inputFile = new File(filePath)

    // Check if file exists
    if (!inputFile.exists()) {
        println "Error: Input file not found at $filePath"
        System.exit(1)
    }

    // Read the Intcode program from the file
    List<Long> program
    try {
        program = inputFile.text.trim().split(',').collect { it.toLong() }
    } catch (NumberFormatException e) {
        println "Error: Invalid number format in input file."
        System.exit(1)
    } catch (IOException e) {
        println "Error reading input file: ${e.message}"
        System.exit(1)
    }


    // Create and run the Intcode computer
    IntcodeComputer computer = new IntcodeComputer(program)
    computer.run() // Part 1 program runs to completion and only outputs

    // Process the ASCII output into a 2D grid
    List<String> grid = []
    StringBuilder currentLine = new StringBuilder()

    computer.outputs.each { outputCode ->
        if (outputCode == 10L) { // ASCII for newline
            // Add the completed line to the grid
            grid << currentLine.toString()
            // Start a new line
            currentLine = new StringBuilder()
        } else {
            // Append character to the current line
            currentLine.append((char) outputCode.intValue())
        }
    }
    // Handle potential last line that might not end with a newline (unlikely for AOC, but good practice)
     if (currentLine.length() > 0) {
         grid << currentLine.toString()
     }

    // Remove any trailing empty line if the output ended with a newline
    if (!grid.isEmpty() && grid.last().isEmpty()) {
        grid.remove(grid.size() - 1)
    }


    // Validate grid dimensions
    int height = grid.size()
    if (height < 3) { // Need at least 3x3 to have a potential intersection
        println "Error: Grid height is too small ($height) to contain intersections."
        // Print grid for debugging if small
        grid.each { println it }
        System.exit(1)
    }
    int width = grid[0].size()
     if (width < 3) { // Need at least 3x3
         println "Error: Grid width is too small ($width) to contain intersections."
         // Print grid for debugging if small
         grid.each { println it }
         System.exit(1)
     }
    // Ensure all rows have the same width
    if (!grid.every { it.size() == width }) {
         println "Error: Grid rows have inconsistent widths."
         System.exit(1)
    }

    // Helper to check if a coordinate contains a scaffold ('#', '^', 'v', '<', '>')
    def isScaffold = { int x, int y ->
        // Check bounds first
        if (x < 0 || x >= width || y < 0 || y >= height) {
            return false // Out of bounds is not a scaffold
        }
        char c = grid[y][x]
        // Scaffold characters include '#', and the robot '^', 'v', '<', '>' (robot is on a scaffold)
        return c == '#' || c == '^' || c == 'v' || c == '<' || c == '>'
    }

    // Find intersections and calculate the sum of alignment parameters
    long sumOfAlignmentParameters = 0

    // Iterate through the potential interior points (cannot be on the border)
    for (int y = 1; y < height - 1; y++) {
        for (int x = 1; x < width - 1; x++) {
            // A point is an intersection if it is a scaffold and all its 4 neighbors are scaffolds
            if (isScaffold(x, y) &&
                isScaffold(x - 1, y) && // Left neighbor
                isScaffold(x + 1, y) && // Right neighbor
                isScaffold(x, y - 1) && // Up neighbor
                isScaffold(x, y + 1))   // Down neighbor
            {
                // Found an intersection at (x, y)
                // Alignment parameter is x * y (0-indexed)
                sumOfAlignmentParameters += (long)x * y
            }
        }
    }

    // Print the final result
    println sumOfAlignmentParameters
}
