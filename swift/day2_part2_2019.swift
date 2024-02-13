
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
var program = input.split(separator: ",").map { Int($0)! }

func runProgram(noun: Int, verb: Int) -> Int {
    var memory = program
    memory[1] = noun
    memory[2] = verb

    var instructionPointer = 0

    while memory[instructionPointer] != 99 {
        let opcode = memory[instructionPointer]
        let input1 = memory[memory[instructionPointer + 1]]
        let input2 = memory[memory[instructionPointer + 2]]
        let outputPosition = memory[instructionPointer + 3]

        if opcode == 1 {
            memory[outputPosition] = input1 + input2
        } else if opcode == 2 {
            memory[outputPosition] = input1 * input2
        }

        instructionPointer += 4
    }

    return memory[0]
}

let targetOutput = 19690720
var result = 0

for noun in 0...99 {
    for verb in 0...99 {
        if runProgram(noun: noun, verb: verb) == targetOutput {
            result = 100 * noun + verb
            break
        }
    }
}

print(result)
