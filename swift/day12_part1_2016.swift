
import Foundation

var registers = ["a": 0, "b": 0, "c": 0, "d": 0]
var instructions = try String(contentsOfFile: "input.txt").components(separatedBy: .newlines)

func getValue(_ input: String) -> Int {
    if let value = Int(input) {
        return value
    } else {
        return registers[input]!
    }
}

var index = 0
while index < instructions.count {
    let parts = instructions[index].components(separatedBy: " ")
    switch parts[0] {
    case "cpy":
        if let value = Int(parts[1]) {
            registers[parts[2]] = value
        } else {
            registers[parts[2]] = registers[parts[1]]!
        }
    case "inc":
        registers[parts[1]]! += 1
    case "dec":
        registers[parts[1]]! -= 1
    case "jnz":
        if getValue(parts[1]) != 0 {
            index += Int(parts[2])! - 1
        }
    default:
        break
    }
    index += 1
}

print(registers["a"]!)
