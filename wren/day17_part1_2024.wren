
import "io" for File

var content = File.read("input.txt")
var lines = content.split("\n")

var regA = 0
var regB = 0
var regC = 0
var program = []

for (rawLine in lines) {
    var line = rawLine
    if (line.endsWith("\r")) line = line[0...-1]
    if (line.startsWith("Register A: ")) {
        regA = Num.fromString(line.split("Register A: ")[1])
    } else if (line.startsWith("Register B: ")) {
        regB = Num.fromString(line.split("Register B: ")[1])
    } else if (line.startsWith("Register C: ")) {
        regC = Num.fromString(line.split("Register C: ")[1])
    } else if (line.startsWith("Program: ")) {
        var parts = line.split("Program: ")[1].split(",")
        for (p in parts) {
            program.add(Num.fromString(p))
        }
    }
}

var div2 = Fn.new { |a, b|
    if (b >= 54) return 0
    var denom = 1
    if (b > 0) {
        for (i in 1..b) {
            denom = denom * 2
        }
    }
    return (a / denom).floor
}

var ip = 0
var outputs = []

while (ip < program.count) {
    var opcode = program[ip]
    var operand = program[ip+1]
    
    var combo = operand
    if (operand == 4) {
        combo = regA
    } else if (operand == 5) {
        combo = regB
    } else if (operand == 6) {
        combo = regC
    }
    
    if (opcode == 0) {
        regA = div2.call(regA, combo)
    } else if (opcode == 1) {
        regB = regB ^ operand
    } else if (opcode == 2) {
        regB = (combo % 8).floor
    } else if (opcode == 3) {
        if (regA != 0) {
            ip = operand
            continue
        }
    } else if (opcode == 4) {
        regB = regB ^ regC
    } else if (opcode == 5) {
        outputs.add((combo % 8).floor)
    } else if (opcode == 6) {
        regB = div2.call(regA, combo)
    } else if (opcode == 7) {
        regC = div2.call(regA, combo)
    }
    
    ip = ip + 2
}

System.print(outputs.join(","))
