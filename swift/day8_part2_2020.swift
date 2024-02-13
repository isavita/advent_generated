
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL, encoding: .utf8)
var instructions = input.components(separatedBy: .newlines)

func runProgram(instructions: [String]) -> (Int, Bool) {
    var accumulator = 0
    var index = 0
    var visited = Set<Int>()

    while index < instructions.count {
        if visited.contains(index) {
            return (accumulator, false)
        }
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

    return (accumulator, true)
}

func fixProgram(instructions: [String]) -> Int {
    for i in 0..<instructions.count {
        var modifiedInstructions = instructions
        let parts = instructions[i].components(separatedBy: " ")

        if parts[0] == "jmp" {
            modifiedInstructions[i] = "nop \(parts[1])"
        } else if parts[0] == "nop" {
            modifiedInstructions[i] = "jmp \(parts[1])"
        }

        let result = runProgram(instructions: modifiedInstructions)
        if result.1 {
            return result.0
        }
    }

    return -1
}

let part1 = runProgram(instructions: instructions)
let part2 = fixProgram(instructions: instructions)

print(part1.0)
print(part2)
