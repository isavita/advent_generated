
import Foundation

// Intcode computer implementation (assuming you have this from previous challenges)
class IntcodeComputer {
    var memory: [Int: Int]
    var ip: Int
    var relativeBase: Int
    var isHalted: Bool
    var inputs: [Int]
    var outputs: [Int]

    init(program: [Int], inputs: [Int] = []) {
        self.memory = [:]
        for (index, value) in program.enumerated() {
            self.memory[index] = value
        }
        self.ip = 0
        self.relativeBase = 0
        self.isHalted = false
        self.inputs = inputs
        self.outputs = []
    }

    func run() {
        while !isHalted {
            let instruction = memory[ip, default: 0]
            let opcode = instruction % 100

            switch opcode {
            case 1: // Add
                execute(instruction: instruction, operation: +)
            case 2: // Multiply
                execute(instruction: instruction, operation: *)
            case 3: // Input
                input(instruction: instruction)
            case 4: // Output
                output(instruction: instruction)
            case 5: // Jump-if-true
                jumpIfTrue(instruction: instruction)
            case 6: // Jump-if-false
                jumpIfFalse(instruction: instruction)
            case 7: // Less than
                lessThan(instruction: instruction)
            case 8: // Equals
                equals(instruction: instruction)
            case 9: // Adjust relative base
                adjustRelativeBase(instruction: instruction)
            case 99: // Halt
                isHalted = true
            default:
                fatalError("Invalid opcode: \(opcode)")
            }
        }
    }


    private func getParameterValue(mode: Int, offset: Int) -> Int {
        let address: Int
        switch mode {
        case 0: // Position mode
            address = memory[ip + offset, default: 0]
        case 1: // Immediate mode
            address = ip + offset
        case 2: // Relative mode
            address = memory[ip + offset, default: 0] + relativeBase
        default:
            fatalError("Invalid parameter mode: \(mode)")
        }
        return memory[address, default: 0]
    }

    private func getWriteAddress(mode: Int, offset: Int) -> Int {
        switch mode {
        case 0: // Position mode
            return memory[ip + offset, default: 0]
        case 2: // Relative mode
            return memory[ip + offset, default: 0] + relativeBase
        default:
            fatalError("Invalid parameter mode for writing: \(mode)")
        }
    }

        private func execute(instruction: Int, operation: (Int, Int) -> Int) {
        let mode1 = (instruction / 100) % 10
        let mode2 = (instruction / 1000) % 10
        let mode3 = (instruction / 10000) % 10

        let param1 = getParameterValue(mode: mode1, offset: 1)
        let param2 = getParameterValue(mode: mode2, offset: 2)
        let writeAddress = getWriteAddress(mode: mode3, offset: 3)

        memory[writeAddress] = operation(param1, param2)
        ip += 4
    }

    private func input(instruction: Int) {
           let mode = (instruction / 100) % 10
           let writeAddress = getWriteAddress(mode: mode, offset: 1)

           if let inputValue = inputs.first {
               memory[writeAddress] = inputValue
               inputs.removeFirst()
               ip += 2
           } else {
              // Handle case of no input.
              fatalError("No input available.")
           }
       }

      private func output(instruction: Int) {
        let mode = (instruction / 100) % 10
        let param = getParameterValue(mode: mode, offset: 1)
        outputs.append(param)
        ip += 2
    }

    private func jumpIfTrue(instruction: Int) {
        let mode1 = (instruction / 100) % 10
        let mode2 = (instruction / 1000) % 10
        let param1 = getParameterValue(mode: mode1, offset: 1)
        let param2 = getParameterValue(mode: mode2, offset: 2)

        if param1 != 0 {
            ip = param2
        } else {
            ip += 3
        }
    }

    private func jumpIfFalse(instruction: Int) {
        let mode1 = (instruction / 100) % 10
        let mode2 = (instruction / 1000) % 10
        let param1 = getParameterValue(mode: mode1, offset: 1)
        let param2 = getParameterValue(mode: mode2, offset: 2)

        if param1 == 0 {
            ip = param2
        } else {
            ip += 3
        }
    }

    private func lessThan(instruction: Int) {
        let mode1 = (instruction / 100) % 10
        let mode2 = (instruction / 1000) % 10
        let mode3 = (instruction / 10000) % 10
        let param1 = getParameterValue(mode: mode1, offset: 1)
        let param2 = getParameterValue(mode: mode2, offset: 2)
        let writeAddress = getWriteAddress(mode: mode3, offset: 3)

        memory[writeAddress] = (param1 < param2) ? 1 : 0
        ip += 4
    }

    private func equals(instruction: Int) {
        let mode1 = (instruction / 100) % 10
        let mode2 = (instruction / 1000) % 10
        let mode3 = (instruction / 10000) % 10

        let param1 = getParameterValue(mode: mode1, offset: 1)
        let param2 = getParameterValue(mode: mode2, offset: 2)
        let writeAddress = getWriteAddress(mode: mode3, offset: 3)

        memory[writeAddress] = (param1 == param2) ? 1 : 0
        ip += 4
    }

    private func adjustRelativeBase(instruction: Int) {
        let mode = (instruction / 100) % 10
        let param = getParameterValue(mode: mode, offset: 1)
        relativeBase += param
        ip += 2
    }
}


func solve() {
    // Read the input file
    guard let input = try? String(contentsOfFile: "input.txt", encoding: .utf8) else {
        fatalError("Could not read input file")
    }
    let program = input.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: ",").compactMap { Int($0) }

    // Run the Intcode program
    let computer = IntcodeComputer(program: program)
    computer.run()

    // Convert the output to a 2D grid
    let outputString = computer.outputs.map { Character(UnicodeScalar($0)!) }.map{String($0)}.joined()
    
    let grid = outputString.split(separator: "\n").map { Array($0) }

    // Find intersections and calculate alignment parameters
    var alignmentSum = 0
    for row in 1..<grid.count - 1 {
        for col in 1..<grid[row].count - 1 {
            if grid[row][col] == "#" &&
               grid[row - 1][col] == "#" &&
               grid[row + 1][col] == "#" &&
               grid[row][col - 1] == "#" &&
               grid[row][col + 1] == "#" {
                alignmentSum += row * col
            }
        }
    }

    print(alignmentSum)
}


solve()
