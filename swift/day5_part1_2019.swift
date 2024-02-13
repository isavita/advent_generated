
import Foundation

func runProgram(program: [Int]) -> Int {
    var memory = program
    var instructionPointer = 0
    var diagnosticCode = 0

    while instructionPointer < memory.count {
        let opcode = memory[instructionPointer] % 100
        let parameterModes = [memory[instructionPointer] / 100 % 10, memory[instructionPointer] / 1000 % 10, memory[instructionPointer] / 10000 % 10]

        func getValue(parameter: Int, mode: Int) -> Int {
            return mode == 0 ? memory[parameter] : parameter
        }

        switch opcode {
        case 1:
            let value1 = getValue(parameter: memory[instructionPointer + 1], mode: parameterModes[0])
            let value2 = getValue(parameter: memory[instructionPointer + 2], mode: parameterModes[1])
            let destination = memory[instructionPointer + 3]
            memory[destination] = value1 + value2
            instructionPointer += 4
        case 2:
            let value1 = getValue(parameter: memory[instructionPointer + 1], mode: parameterModes[0])
            let value2 = getValue(parameter: memory[instructionPointer + 2], mode: parameterModes[1])
            let destination = memory[instructionPointer + 3]
            memory[destination] = value1 * value2
            instructionPointer += 4
        case 3:
            let destination = memory[instructionPointer + 1]
            memory[destination] = 1 // Input value
            instructionPointer += 2
        case 4:
            let output = getValue(parameter: memory[instructionPointer + 1], mode: parameterModes[0])
            diagnosticCode = output
            instructionPointer += 2
        case 99:
            return diagnosticCode
        default:
            break
        }
    }

    return diagnosticCode
}

if let input = try? String(contentsOfFile: "input.txt") {
    let program = input.components(separatedBy: ",").compactMap { Int($0) }
    let result = runProgram(program: program)
    print(result)
}
