
def program = new File("input.txt").readLines()
def registers = [a: 0, b: 0, c: 1, d: 0]
int index = 0

while (index < program.size()) {
    def instruction = program[index].tokenize()
    def cmd = instruction[0]
    def x = instruction[1]
    def y = instruction.size() > 2 ? instruction[2] : null

    switch (cmd) {
        case "cpy":
            registers[y] = registers.containsKey(x) ? registers[x] : x.toInteger()
            break
        case "inc":
            registers[x]++
            break
        case "dec":
            registers[x]--
            break
        case "jnz":
            def value = registers.containsKey(x) ? registers[x] : x.toInteger()
            if (value != 0) {
                index += y.toInteger() - 1
            }
            break
    }

    index++
}

println registers.a
