
import Foundation

var registers: [String: Int] = ["a": 0, "b": 0, "c": 0, "d": 0, "e": 0, "f": 0, "g": 0, "h": 0]
var instructions: [String] = []

if let input = try? String(contentsOfFile: "input.txt") {
    instructions = input.components(separatedBy: .newlines)
}

var mulCount = 0
var index = 0

while index < instructions.count {
    let parts = instructions[index].components(separatedBy: " ")

    let register = parts[1]
    let value = Int(parts[2]) ?? (registers[parts[2]] ?? 0)

    switch parts[0] {
    case "set":
        registers[register] = value
    case "sub":
        registers[register]! -= value
    case "mul":
        registers[register]! *= value
        mulCount += 1
    case "jnz":
        let check = Int(register) ?? (registers[register] ?? 0)
        if check != 0 {
            index += value
            continue
        }
    default:
        break
    }

    index += 1
}

print(mulCount)
