
def instructions = new File("input.txt").readLines()
def registers = ["a": 0, "b": 0]

for (int i = 0; i < instructions.size(); i++) {
    def parts = instructions[i].split(" ")

    switch (parts[0]) {
        case "hlf":
            registers[parts[1]] /= 2
            break
        case "tpl":
            registers[parts[1]] *= 3
            break
        case "inc":
            registers[parts[1]]++
            break
        case "jmp":
            i += parts[1].toInteger() - 1
            break
        case "jie":
            if (registers[parts[1][0]] % 2 == 0) {
                i += parts[2].toInteger() - 1
            }
            break
        case "jio":
            if (registers[parts[1][0]] == 1) {
                i += parts[2].toInteger() - 1
            }
            break
        default:
            throw new RuntimeException("Unknown instruction: ${parts[0]}")
    }
}

println registers["b"]
