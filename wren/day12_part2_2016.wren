import "io" for File

var getValue = Fn.new { |s, registers|
    var val = Num.fromString(s)
    if (val == null) {
        return registers[s]
    }
    return val
}

var main = Fn.new {
    var input = File.read("input.txt")
    var instructions = input.split("\n")

    var registers = Map.new()
    registers["a"] = 0
    registers["b"] = 0
    registers["c"] = 1
    registers["d"] = 0

    var i = 0
    while (i < instructions.count) {
        var parts = instructions[i].split(" ")
        var cmd = parts[0]
        if (cmd == "cpy") {
            var val = getValue.call(parts[1], registers)
            registers[parts[2]] = val
            i = i + 1
        } else if (cmd == "inc") {
            registers[parts[1]] = registers[parts[1]] + 1
            i = i + 1
        } else if (cmd == "dec") {
            registers[parts[1]] = registers[parts[1]] - 1
            i = i + 1
        } else if (cmd == "jnz") {
            var val = getValue.call(parts[1], registers)
            if (val != 0) {
                var jump = Num.fromString(parts[2])
                i = i + jump
            } else {
                i = i + 1
            }
        }
    }

    System.print(registers["a"])
}

main.call()