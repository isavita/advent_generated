
import Foundation

var registers = ["a": 7, "b": 0, "c": 0, "d": 0]
var instructions = try String(contentsOfFile: "input.txt").components(separatedBy: .newlines)

func getValue(_ input: String) -> Int {
    if let value = Int(input) {
        return value
    } else {
        return registers[input] ?? 0
    }
}

var i = 0
while i < instructions.count {
    let parts = instructions[i].components(separatedBy: " ")
    switch parts[0] {
    case "cpy":
        let value = getValue(parts[1])
        registers[parts[2]] = value
    case "inc":
        registers[parts[1]]! += 1
    case "dec":
        registers[parts[1]]! -= 1
    case "jnz":
        let check = getValue(parts[1])
        let jump = getValue(parts[2])
        if check != 0 {
            i += jump - 1
        }
    case "tgl":
        let toggleIndex = i + getValue(parts[1])
        if toggleIndex < instructions.count {
            let toggleParts = instructions[toggleIndex].components(separatedBy: " ")
            if toggleParts.count == 2 {
                if toggleParts[0] == "inc" {
                    instructions[toggleIndex] = "dec \(toggleParts[1])"
                } else {
                    instructions[toggleIndex] = "inc \(toggleParts[1])"
                }
            } else if toggleParts.count == 3 {
                if toggleParts[0] == "jnz" {
                    instructions[toggleIndex] = "cpy \(toggleParts[1]) \(toggleParts[2])"
                } else {
                    instructions[toggleIndex] = "jnz \(toggleParts[1]) \(toggleParts[2])"
                }
            }
        }
    default:
        break
    }
    i += 1
}

print(registers["a"]!)
