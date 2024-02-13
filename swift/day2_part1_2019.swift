
import Foundation

let input = try String(contentsOfFile: "input.txt")
var program = input.components(separatedBy: ",").compactMap { Int($0) }

program[1] = 12
program[2] = 2

var index = 0
while program[index] != 99 {
    let opcode = program[index]
    let input1 = program[program[index + 1]]
    let input2 = program[program[index + 2]]
    let outputIndex = program[index + 3]

    if opcode == 1 {
        program[outputIndex] = input1 + input2
    } else if opcode == 2 {
        program[outputIndex] = input1 * input2
    }

    index += 4
}

print(program[0])
