
import Foundation

// Read input from the file
func readInput(from file: String) -> [Int] {
    let url = URL(fileURLWithPath: file)
    let input = try! String(contentsOf: url)
    return input.split(separator: ",").compactMap { Int($0) }
}

// Intcode computer implementation
class IntcodeComputer {
    var memory: [Int]
    var pointer: Int = 0
    var relativeBase: Int = 0

    init(program: [Int]) {
        // Extend memory to accommodate larger values
        self.memory = program + Array(repeating: 0, count: 10000)
    }

    func getParameterValue(at index: Int, mode: Int) -> Int {
        switch mode {
        case 0: // Position mode
            return memory[memory[index]]
        case 1: // Immediate mode
            return memory[index]
        case 2: // Relative mode
            return memory[relativeBase + memory[index]]
        default:
            fatalError("Unknown parameter mode")
        }
    }

    func getWriteAddress(at index: Int, mode: Int) -> Int {
        switch mode {
        case 0: // Position mode
            return memory[index]
        case 2: // Relative mode
            return relativeBase + memory[index]
        default:
            fatalError("Invalid mode for writing")
        }
    }

    func run(input: Int) -> Int? {
        var output: Int?
        while true {
            let instruction = memory[pointer]
            let opcode = instruction % 100
            let modes = [
                (instruction / 100) % 10,
                (instruction / 1000) % 10,
                (instruction / 10000) % 10
            ]

            switch opcode {
            case 1: // Add
                let a = getParameterValue(at: pointer + 1, mode: modes[0])
                let b = getParameterValue(at: pointer + 2, mode: modes[1])
                let address = getWriteAddress(at: pointer + 3, mode: modes[2])
                memory[address] = a + b
                pointer += 4

            case 2: // Multiply
                let c = getParameterValue(at: pointer + 1, mode: modes[0])
                let d = getParameterValue(at: pointer + 2, mode: modes[1])
                let address2 = getWriteAddress(at: pointer + 3, mode: modes[2])
                memory[address2] = c * d
                pointer += 4

            case 3: // Input
                let address3 = getWriteAddress(at: pointer + 1, mode: modes[0])
                memory[address3] = input
                pointer += 2

            case 4: // Output
                output = getParameterValue(at: pointer + 1, mode: modes[0])
                pointer += 2

            case 5: // Jump-if-true
                let e = getParameterValue(at: pointer + 1, mode: modes[0])
                if e != 0 {
                    pointer = getParameterValue(at: pointer + 2, mode: modes[1])
                } else {
                    pointer += 3
                }

            case 6: // Jump-if-false
                let f = getParameterValue(at: pointer + 1, mode: modes[0])
                if f == 0 {
                    pointer = getParameterValue(at: pointer + 2, mode: modes[1])
                } else {
                    pointer += 3
                }

            case 7: // Less than
                let g = getParameterValue(at: pointer + 1, mode: modes[0])
                let h = getParameterValue(at: pointer + 2, mode: modes[1])
                let address4 = getWriteAddress(at: pointer + 3, mode: modes[2])
                memory[address4] = (g < h) ? 1 : 0
                pointer += 4

            case 8: // Equals
                let i = getParameterValue(at: pointer + 1, mode: modes[0])
                let j = getParameterValue(at: pointer + 2, mode: modes[1])
                let address5 = getWriteAddress(at: pointer + 3, mode: modes[2])
                memory[address5] = (i == j) ? 1 : 0
                pointer += 4

            case 9: // Adjust relative base
                let k = getParameterValue(at: pointer + 1, mode: modes[0])
                relativeBase += k
                pointer += 2

            case 99: // Halt
                return output

            default:
                fatalError("Unknown opcode \(opcode)")
            }
        }
    }
}

// Main execution
let program = readInput(from: "input.txt")
let computer = IntcodeComputer(program: program)
if let boostKeycode = computer.run(input: 1) {
    print("BOOST keycode: \(boostKeycode)")
} else {
    print("No output produced.")
}
