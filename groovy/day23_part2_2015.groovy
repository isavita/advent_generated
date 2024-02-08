
def data = new File("input.txt").text.trim().split("\n")

def registers = ["a": 1, "b": 0]

for (int i = 0; i < data.size(); i++) {
    def parts = data[i].tokenize()

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
            i += parts[1] as int - 1
            break
        case "jie":
            if (registers[parts[1][0]] % 2 == 0) {
                i += parts[2] as int - 1
            }
            break
        case "jio":
            if (registers[parts[1][0]] == 1) {
                i += parts[2] as int - 1
            }
            break
        default:
            throw new RuntimeException("Unknown instruction: ${parts[0]}")
    }
}

println(registers["b"])
