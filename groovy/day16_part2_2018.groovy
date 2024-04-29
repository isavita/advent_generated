def input = new File('input.txt').text.trim()
def lines = input.split('\n')

def opcodes = [
    [name: 'addr', action: '+', a: 'r', b: 'r'],
    [name: 'addi', action: '+', a: 'r', b: 'v'],
    [name: 'mulr', action: '*', a: 'r', b: 'r'],
    [name: 'muli', action: '*', a: 'r', b: 'v'],
    [name: 'banr', action: '&', a: 'r', b: 'r'],
    [name: 'bani', action: '&', a: 'r', b: 'v'],
    [name: 'borr', action: '|', a: 'r', b: 'r'],
    [name: 'bori', action: '|', a: 'r', b: 'v'],
    [name: 'setr', action: 'a', a: 'r', b: 'r'],
    [name: 'seti', action: 'a', a: 'v', b: 'r'],
    [name: 'gtir', action: '>', a: 'v', b: 'r'],
    [name: 'gtri', action: '>', a: 'r', b: 'v'],
    [name: 'gtrr', action: '>', a: 'r', b: 'r'],
    [name: 'eqir', action: '=', a: 'v', b: 'r'],
    [name: 'eqri', action: '=', a: 'r', b: 'v'],
    [name: 'eqrr', action: '=', a: 'r', b: 'r']
]

for (opcode in opcodes) {
    opcode.matchCount = []
}

def sum = 0
def lineCount = 0
while (lineCount < lines.size()) {
    if (lines[lineCount].size() > 0 && lines[lineCount][0] == 'B') {
        def split = lines[lineCount].split('[^0-9]+')
        def registers = [split[1].toInteger(), split[2].toInteger(), split[3].toInteger(), split[4].toInteger()]
        split = lines[lineCount + 1].split('[^0-9]+')
        def instruction = [split[0].toInteger(), split[1].toInteger(), split[2].toInteger(), split[3].toInteger()]
        split = lines[lineCount + 2].split('[^0-9]+')
        def result = [split[1].toInteger(), split[2].toInteger(), split[3].toInteger(), split[4].toInteger()]
        def tempSum = testCode(registers, result, instruction, opcodes)
        if (tempSum >= 3) {
            sum++
        }
        lineCount += 4
    } else {
        break
    }
}

def orderedOpCodes = [:]
while (orderedOpCodes.size() < 16) {
    for (opcode in opcodes) {
        if (opcode.matchCount.size() == 1) {
            def c = opcode.matchCount[0]
            orderedOpCodes[c] = opcode
            for (op in opcodes) {
                remove(op, c)
            }
        }
    }
}

lineCount += 2

def r = [0, 0, 0, 0]

while (lineCount < lines.size()) {
    def split = lines[lineCount].split('[^0-9]+')
    def instruction = [split[0].toInteger(), split[1].toInteger(), split[2].toInteger(), split[3].toInteger()]
    r = runOp(orderedOpCodes[instruction[0]], r, instruction)
    lineCount++
}

println r[0]

def remove(def op, def c) {
    def i = -1
    for (def j = 0; j < op.matchCount.size(); j++) {
        if (op.matchCount[j] == c) {
            i = j
        }
    }
    if (i != -1) {
        op.matchCount = op.matchCount[0..<i] + op.matchCount[i + 1..<op.matchCount.size()]
    }
}

def add(def op, def c) {
    for (def v : op.matchCount) {
        if (v == c) {
            return
        }
    }
    op.matchCount << c
}

def testCode(def registers, def result, def instruction, def opcodes) {
    def sum = 0
    for (def opcode : opcodes) {
        if (match(result, runOp(opcode, registers, instruction))) {
            add(opcode, instruction[0])
            sum++
        }
    }
    return sum
}

def match(def r, def c) {
    if (r.size() != c.size()) {
        return false
    }
    for (def i = 0; i < r.size(); i++) {
        if (r[i] != c[i]) {
            return false
        }
    }
    return true
}

def runOp(def op, def registers, def instruction) {
    def registerCP = registers.clone()
    def A, B
    if (op.a == 'r') {
        A = registerCP[instruction[1]]
    } else {
        A = instruction[1]
    }
    if (op.b == 'r') {
        B = registerCP[instruction[2]]
    } else {
        B = instruction[2]
    }
    switch (op.action) {
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
        default:
            println "not valid instruction"
    }
    return registerCP
}