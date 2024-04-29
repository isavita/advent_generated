import static java.nio.file.Files.readAllLines
import static java.nio.file.Paths.get

def input = readAllLines(get("input.txt")).collect { it.trim() }

def opcodes = [
    [name: "addr", action: '+', a: 'r', b: 'r'],
    [name: "addi", action: '+', a: 'r', b: 'v'],
    [name: "mulr", action: '*', a: 'r', b: 'r'],
    [name: "muli", action: '*', a: 'r', b: 'v'],
    [name: "banr", action: '&', a: 'r', b: 'r'],
    [name: "bani", action: '&', a: 'r', b: 'v'],
    [name: "borr", action: '|', a: 'r', b: 'r'],
    [name: "bori", action: '|', a: 'r', b: 'v'],
    [name: "setr", action: 'a', a: 'r', b: 'r'],
    [name: "seti", action: 'a', a: 'v', b: 'r'],
    [name: "gtir", action: '>', a: 'v', b: 'r'],
    [name: "gtri", action: '>', a: 'r', b: 'v'],
    [name: "gtrr", action: '>', a: 'r', b: 'r'],
    [name: "eqir", action: '=', a: 'v', b: 'r'],
    [name: "eqri", action: '=', a: 'r', b: 'v'],
    [name: "eqrr", action: '=', a: 'r', b: 'r']
]

def sum = 0
def lineCount = 0
while (lineCount < input.size()) {
    if (input[lineCount].startsWith("Before")) {
        def registers = input[lineCount].split("\\[")[1].split("]")[0].split(", ").collect { it.toInteger() }
        def instruction = input[lineCount + 1].split(" ").collect { it.toInteger() }
        def n = input[lineCount + 2].split("\\[")[1].split("]")[0].split(", ").collect { it.toInteger() }
        def tempSum = testCode(registers, n, instruction, opcodes)
        if (tempSum >= 3) {
            sum++
        }
        lineCount += 4
    } else {
        break
    }
}

println sum

def testCode(registers, n, instruction, opcodes) {
    def sum = 0
    opcodes.each { opcode ->
        if (match(n, runOp(opcode, registers, instruction))) {
            sum++
        }
    }
    sum
}

def match(r, c) {
    if (r.size() != c.size()) {
        return false
    }
    for (int i = 0; i < r.size(); i++) {
        if (r[i] != c[i]) {
            return false
        }
    }
    true
}

def runOp(opcode, registers, instruction) {
    def registerCP = registers.clone()
    def A, B
    if (opcode.a == 'r') {
        A = registerCP[instruction[1]]
    } else {
        A = instruction[1]
    }
    if (opcode.b == 'r') {
        B = registerCP[instruction[2]]
    } else {
        B = instruction[2]
    }
    switch (opcode.action) {
        case '+':
            registerCP[instruction[3]] = A + B
            break
        case '*':
            registerCP[instruction[3]] = A * B
            break
        case '&':
            registerCP[instruction[3]] = A & B
            break
        case '|':
            registerCP[instruction[3]] = A | B
            break
        case 'a':
            registerCP[instruction[3]] = A
            break
        case '>':
            registerCP[instruction[3]] = A > B ? 1 : 0
            break
        case '=':
            registerCP[instruction[3]] = A == B ? 1 : 0
            break
    }
    registerCP
}