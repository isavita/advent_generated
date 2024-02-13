
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)
var instructions = input.components(separatedBy: "\n")

var accumulator = 0
var index = 0
var visited = Set<Int>()

while !visited.contains(index) {
    visited.insert(index)
    let parts = instructions[index].components(separatedBy: " ")
    let operation = parts[0]
    let argument = Int(parts[1])!

    switch operation {
    case "acc":
        accumulator += argument
        index += 1
    case "jmp":
        index += argument
    case "nop":
        index += 1
    default:
        break
    }
}

print(accumulator)
