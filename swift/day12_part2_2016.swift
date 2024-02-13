
import Foundation

var registers = ["a": 0, "b": 0, "c": 1, "d": 0]
var instructions = try String(contentsOfFile: "input.txt").components(separatedBy: .newlines)

func getValue(_ input: String) -> Int {
    if let value = Int(input) {
        return value
    } else {
        return registers[input]!
    }
}

var i = 0
while i < instructions.count {
    let parts = instructions[i].components(separatedBy: " ")
    switch parts[0] {
    case "cpy":
        registers[parts[2]] = getValue(parts[1])
    case "inc":
        registers[parts[1]]! += 1
    case "dec":
        registers[parts[1]]! -= 1
    case "jnz":
        if getValue(parts[1]) != 0 {
            i += Int(parts[2])! - 1
        }
    default:
        break
    }
    i += 1
}

print(registers["a"]!)
