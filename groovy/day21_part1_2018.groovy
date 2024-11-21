
def input = new File('input.txt').text
def lines = input.split('\n')
def instructionPointer = lines[0].split(' ')[1].toInteger()
def instructions = lines[1..-1].collect { line ->
    def parts = line.split(' ')
    [name: parts[0], abcValues: parts[1..3].collect { it.toInteger() }]
}

def registers = [0, 0, 0, 0, 0, 0]

def opcodeNamesToFuncs = [
    addr: { r, a, b, c -> r[c] = r[a] + r[b]; r },
    addi: { r, a, b, c -> r[c] = r[a] + b; r },
    mulr: { r, a, b, c -> r[c] = r[a] * r[b]; r },
    muli: { r, a, b, c -> r[c] = r[a] * b; r },
    banr: { r, a, b, c -> r[c] = r[a] & r[b]; r },
    bani: { r, a, b, c -> r[c] = r[a] & b; r },
    borr: { r, a, b, c -> r[c] = r[a] | r[b]; r },
    bori: { r, a, b, c -> r[c] = r[a] | b; r },
    setr: { r, a, b, c -> r[c] = r[a]; r },
    seti: { r, a, b, c -> r[c] = a; r },
    gtir: { r, a, b, c -> r[c] = a > r[b] ? 1 : 0; r },
    gtri: { r, a, b, c -> r[c] = r[a] > b ? 1 : 0; r },
    gtrr: { r, a, b, c -> r[c] = r[a] > r[b] ? 1 : 0; r },
    eqir: { r, a, b, c -> r[c] = a == r[b] ? 1 : 0; r },
    eqri: { r, a, b, c -> r[c] = r[a] == b ? 1 : 0; r },
    eqrr: { r, a, b, c -> r[c] = r[a] == r[b] ? 1 : 0; r }
]

while (registers[instructionPointer] < instructions.size()) {
    def inst = instructions[registers[instructionPointer]]
    opcodeNamesToFuncs[inst.name](registers, inst.abcValues[0], inst.abcValues[1], inst.abcValues[2])
    registers[instructionPointer]++
    if (registers[instructionPointer] == 28) break
}

println registers[5]
