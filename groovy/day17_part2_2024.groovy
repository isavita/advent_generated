
import java.io.File
import java.io.FileNotFoundException

static long computeOperand(long val, long a, long b, long c) {
    switch (val) {
        case 0L: return 0L
        case 1L: return 1L
        case 2L: return 2L
        case 3L: return 3L
        case 4L: return a
        case 5L: return b
        case 6L: return c
        default:
            throw new IllegalArgumentException("Invalid combo operand value: ${val}")
    }
}

static List<Integer> simulateComputer(long initialA, long initialB, long initialC, List<Integer> instructions) {
    long a = initialA
    long b = initialB
    long c = initialC
    List<Integer> outs = []

    int pc = 0
    while (pc < instructions.size()) {
        int cmd = instructions[pc]
        long operand = (pc + 1 < instructions.size()) ? (long)instructions[pc+1] : 0L

        switch (cmd) {
            case 0: a >>= computeOperand(operand, a, b, c)
                    pc += 2
                    break
            case 1: b ^= operand
                    pc += 2
                    break
            case 2: b = computeOperand(operand, a, b, c) % 8
                    pc += 2
                    break
            case 3: if (a != 0) {
                        pc = (int)operand
                    } else {
                        pc += 2
                    }
                    break
            case 4: b ^= c
                    pc += 2
                    break
            case 5: outs << (int)(computeOperand(operand, a, b, c) % 8)
                    pc += 2
                    break
            case 6: b = a >> computeOperand(operand, a, b, c)
                    pc += 2
                    break
            case 7: c = a >> computeOperand(operand, a, b, c)
                    pc += 2
                    break
            default:
                throw new IllegalArgumentException("Invalid opcode: ${cmd} at pc=${pc}")
        }
    }
    return outs
}

static List<Long> check(long initialB, long initialC, List<Integer> instructions) {
    List<Long> valids = []
    List<List<Long>> stack = [[0L, 0L]]
    Map<List<Long>, Boolean> seen = [:]

    while (!stack.isEmpty()) {
        List<Long> state = stack.pop()
        long depth = state[0]
        long score = state[1]

        if (seen.containsKey(state)) {
            continue
        }
        seen[state] = true

        if (depth == instructions.size()) {
            valids << score
        } else {
            for (long i = 0L; i < 8L; i++) {
                long newScore = score * 8L + i

                List<Integer> result = simulateComputer(newScore, initialB, initialC, instructions)

                if (result.size() > 0 && result[0] == instructions[instructions.size() - 1 - (int)depth]) {
                     stack.push([depth + 1L, newScore])
                }
            }
        }
    }
    return valids
}

long initialA = 0
long initialB = 0
long initialC = 0
List<Integer> instructions = []

File inputFile = new File("input.txt")
if (!inputFile.exists()) {
    throw new FileNotFoundException("input.txt not found.")
}

inputFile.eachLine { line ->
    line = line.trim()
    if (line.startsWith("Register A:")) {
        initialA = line.split(":")[1].trim() as long
    } else if (line.startsWith("Register B:")) {
        initialB = line.split(":")[1].trim() as long
    } else if (line.startsWith("Register C:")) {
        initialC = line.split(":")[1].trim() as long
    } else if (line.startsWith("Program:")) {
        instructions = line.split(":")[1].trim().split(",").collect { it.trim() as int }
    }
}

List<Long> validValues = check(initialB, initialC, instructions)

if (validValues.isEmpty()) {
    throw new RuntimeException("No valid values found.")
} else {
    println validValues.min()
}
