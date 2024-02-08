
def instructions = new File("input.txt").readLines()
def registers = [:].withDefault { 0 }
def idx = 0
def mulInvoked = 0

while (idx >= 0 && idx < instructions.size()) {
    def parts = instructions[idx].split()
    def op = parts[0]
    def X = parts[1]
    def Y = parts.size() > 2 ? parts[2] : null

    switch (op) {
        case "set":
            registers[X] = Y.isNumber() ? Y.toInteger() : registers[Y]
            break
        case "sub":
            registers[X] -= Y.isNumber() ? Y.toInteger() : registers[Y]
            break
        case "mul":
            registers[X] *= Y.isNumber() ? Y.toInteger() : registers[Y]
            mulInvoked++
            break
        case "jnz":
            def check = X.isNumber() ? X.toInteger() : registers[X]
            def offset = Y.isNumber() ? Y.toInteger() : registers[Y]
            if (check != 0) {
                idx += offset
                continue
            }
            break
    }

    idx++
}

println mulInvoked
