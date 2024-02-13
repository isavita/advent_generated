
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let data = try String(contentsOf: fileURL)
let instructions = data.components(separatedBy: "\n")

var registers: [String: Int] = ["a": 1, "b": 0]

var i = 0
while i < instructions.count {
    let parts = instructions[i].components(separatedBy: " ")

    switch parts[0] {
    case "hlf":
        registers[parts[1]]! /= 2
    case "tpl":
        registers[parts[1]]! *= 3
    case "inc":
        registers[parts[1]]! += 1
    case "jmp":
        let offset = Int(parts[1])!
        i += offset - 1
    case "jie":
        if registers[String(parts[1].prefix(1))]! % 2 == 0 {
            let offset = Int(parts[2])!
            i += offset - 1
        }
    case "jio":
        if registers[String(parts[1].prefix(1))]! == 1 {
            let offset = Int(parts[2])!
            i += offset - 1
        }
    default:
        fatalError("Unknown instruction: \(parts[0])")
    }

    i += 1
}

print(registers["b"]!)
