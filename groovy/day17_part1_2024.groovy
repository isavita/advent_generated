
def lines = new File("input.txt").readLines()
int A = 0, B = 0, C = 0
List<Integer> program = []

lines.each { String line ->
    line = line.trim()
    if (!line) return
    if (line.startsWith("Register A:")) {
        A = line.split(":")[1].trim().toInteger()
    } else if (line.startsWith("Register B:")) {
        B = line.split(":")[1].trim().toInteger()
    } else if (line.startsWith("Register C:")) {
        C = line.split(":")[1].trim().toInteger()
    } else if (line.startsWith("Program:")) {
        program = line.split(":")[1].trim().split(",").collect { it.trim().toInteger() }
    }
}

def getComboVal = { int op ->
    switch (op) {
        case 0..3: return op
        case 4: return A
        case 5: return B
        case 6: return C
        default: throw new IllegalArgumentException("invalid combo operand")
    }
}

List<String> outputVals = []
int ip = 0
while (ip < program.size()) {
    int opcode = program[ip]
    if (ip + 1 >= program.size()) break
    int operand = program[ip + 1]

    switch (opcode) {
        case 0:
            int den = getComboVal(operand)
            A = den == 0 ? 0 : A >> den
            ip += 2
            break
        case 1:
            B = B ^ operand
            ip += 2
            break
        case 2:
            B = getComboVal(operand) % 8
            ip += 2
            break
        case 3:
            ip = A != 0 ? operand : ip + 2
            break
        case 4:
            B = B ^ C
            ip += 2
            break
        case 5:
            outputVals << (getComboVal(operand) % 8).toString()
            ip += 2
            break
        case 6:
            B = A >> getComboVal(operand)
            ip += 2
            break
        case 7:
            C = A >> getComboVal(operand)
            ip += 2
            break
        default:
            break
    }
}

println outputVals.join(",")
