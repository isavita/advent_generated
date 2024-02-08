
def file = new File("input.txt")
def instructions = file.readLines()

def producesClockSignal(a, instructions) {
    def registers = ["a": a, "b": 0, "c": 0, "d": 0]
    def lastOutput = 0
    def outputCount = 0

    for (int i = 0; i < instructions.size(); ) {
        def parts = instructions[i].split(" ")
        switch (parts[0]) {
            case "cpy":
                def val = parts[1].isInteger() ? parts[1].toInteger() : registers[parts[1]]
                registers[parts[2]] = val
                break
            case "inc":
                registers[parts[1]]++
                break
            case "dec":
                registers[parts[1]]--
                break
            case "jnz":
                def val = parts[1].isInteger() ? parts[1].toInteger() : registers[parts[1]]
                if (val != 0) {
                    def jump = parts[2].toInteger()
                    i += jump
                    continue
                }
                break
            case "out":
                def val = parts[1].isInteger() ? parts[1].toInteger() : registers[parts[1]]
                if (val != 0 && val != 1) {
                    return false
                }
                if (outputCount > 0 && val == lastOutput) {
                    return false
                }
                lastOutput = val
                outputCount++
                if (outputCount > 50) {
                    return true
                }
                break
        }
        i++
    }
    return false
}

def a = 1
while (true) {
    if (producesClockSignal(a, instructions)) {
        println(a)
        break
    }
    a++
}
