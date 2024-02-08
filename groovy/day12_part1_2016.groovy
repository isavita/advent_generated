def program = new File("input.txt").readLines()
def registers = [a: 0, b: 0, c: 0, d: 0]
def pc = 0

while (pc < program.size()) {
    def parts = program[pc].split()
    switch (parts[0]) {
        case "cpy":
            registers[parts[2]] = isNumber(parts[1]) ? parts[1] as int : registers[parts[1]]
            break
        case "inc":
            registers[parts[1]]++
            break
        case "dec":
            registers[parts[1]]--
            break
        case "jnz":
            def val = isNumber(parts[1]) ? parts[1] as int : registers[parts[1]]
            if (val != 0) {
                pc += parts[2] as int - 1
            }
            break
    }
    pc++
}

println registers.a

def isNumber(String s) {
    return s.isNumber()
}