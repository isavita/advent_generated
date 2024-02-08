
def file = new File("input.txt")
def ipBind
def instructions = []

file.eachLine { line ->
    if (line.startsWith("#ip")) {
        ipBind = line.tokenize(" ")[1] as int
    } else {
        instructions.add(line.tokenize(" "))
    }
}

def registers = [0, 0, 0, 0, 0, 0]

for (int ip = 0; ip < instructions.size(); ip++) {
    registers[ipBind] = ip
    def inst = instructions[ip]
    def opcode = inst[0]
    def a = inst[1] as int
    def b = inst[2] as int
    def c = inst[3] as int

    switch (opcode) {
        case "addr":
            registers[c] = registers[a] + registers[b]
            break
        case "addi":
            registers[c] = registers[a] + b
            break
        case "mulr":
            registers[c] = registers[a] * registers[b]
            break
        case "muli":
            registers[c] = registers[a] * b
            break
        case "banr":
            registers[c] = registers[a] & registers[b]
            break
        case "bani":
            registers[c] = registers[a] & b
            break
        case "borr":
            registers[c] = registers[a] | registers[b]
            break
        case "bori":
            registers[c] = registers[a] | b
            break
        case "setr":
            registers[c] = registers[a]
            break
        case "seti":
            registers[c] = a
            break
        case "gtir":
            registers[c] = a > registers[b] ? 1 : 0
            break
        case "gtri":
            registers[c] = registers[a] > b ? 1 : 0
            break
        case "gtrr":
            registers[c] = registers[a] > registers[b] ? 1 : 0
            break
        case "eqir":
            registers[c] = a == registers[b] ? 1 : 0
            break
        case "eqri":
            registers[c] = registers[a] == b ? 1 : 0
            break
        case "eqrr":
            registers[c] = registers[a] == registers[b] ? 1 : 0
            break
    }

    ip = registers[ipBind]
    if (ip < 0 || ip >= instructions.size()) {
        break
    }
}

println registers[0]
