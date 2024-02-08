
def file = new File("input.txt")
def registers = [:]

file.eachLine { line ->
    def parts = line.split()
    def reg = parts[0]
    def op = parts[1]
    def amount = parts[2] as int
    def condReg = parts[4]
    def condOp = parts[5]
    def condVal = parts[6] as int

    def cond = false
    switch (condOp) {
        case ">":
            cond = registers.getOrDefault(condReg, 0) > condVal
            break
        case ">=":
            cond = registers.getOrDefault(condReg, 0) >= condVal
            break
        case "<":
            cond = registers.getOrDefault(condReg, 0) < condVal
            break
        case "<=":
            cond = registers.getOrDefault(condReg, 0) <= condVal
            break
        case "==":
            cond = registers.getOrDefault(condReg, 0) == condVal
            break
        case "!=":
            cond = registers.getOrDefault(condReg, 0) != condVal
            break
    }

    if (cond) {
        switch (op) {
            case "inc":
                registers[reg] = registers.getOrDefault(reg, 0) + amount
                break
            case "dec":
                registers[reg] = registers.getOrDefault(reg, 0) - amount
                break
        }
    }
}

def maxValue = registers.values().max()
println(maxValue)
