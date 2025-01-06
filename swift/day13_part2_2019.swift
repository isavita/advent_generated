
import Foundation

// Intcode computer
class IntcodeComputer {
    var memory: [Int]
    var ip: Int = 0
    var relativeBase: Int = 0
    var input: [Int] = []
    var output: [Int] = []

    init(program: [Int]) {
        self.memory = program + Array(repeating: 0, count: 1000) // Add extra memory
    }

    func run() {
        while true {
            let opcode = memory[ip] % 100
            let modes = [
                (memory[ip] / 100) % 10,
                (memory[ip] / 1000) % 10,
                (memory[ip] / 10000) % 10
            ]

            switch opcode {
            case 1: // Addition
                let param1 = getParameter(at: 1, mode: modes[0])
                let param2 = getParameter(at: 2, mode: modes[1])
                setParameter(at: 3, mode: modes[2], value: param1 + param2)
                ip += 4
            case 2: // Multiplication
                let param1 = getParameter(at: 1, mode: modes[0])
                let param2 = getParameter(at: 2, mode: modes[1])
                setParameter(at: 3, mode: modes[2], value: param1 * param2)
                ip += 4
            case 3: // Input
                if input.isEmpty { return } // Pause if no input
                setParameter(at: 1, mode: modes[0], value: input.removeFirst())
                ip += 2
            case 4: // Output
                output.append(getParameter(at: 1, mode: modes[0]))
                ip += 2
            case 5: // Jump-if-true
                if getParameter(at: 1, mode: modes[0]) != 0 {
                    ip = getParameter(at: 2, mode: modes[1])
                } else {
                    ip += 3
                }
            case 6: // Jump-if-false
                if getParameter(at: 1, mode: modes[0]) == 0 {
                    ip = getParameter(at: 2, mode: modes[1])
                } else {
                    ip += 3
                }
            case 7: // Less than
                let param1 = getParameter(at: 1, mode: modes[0])
                let param2 = getParameter(at: 2, mode: modes[1])
                setParameter(at: 3, mode: modes[2], value: param1 < param2 ? 1 : 0)
                ip += 4
            case 8: // Equals
                let param1 = getParameter(at: 1, mode: modes[0])
                let param2 = getParameter(at: 2, mode: modes[1])
                setParameter(at: 3, mode: modes[2], value: param1 == param2 ? 1 : 0)
                ip += 4
            case 9: // Adjust relative base
                relativeBase += getParameter(at: 1, mode: modes[0])
                ip += 2
            case 99: // Halt
                return
            default:
                fatalError("Invalid opcode: \(opcode)")
            }
        }
    }

    private func getParameter(at offset: Int, mode: Int) -> Int {
        switch mode {
        case 0: return memory[memory[ip + offset]] // Position mode
        case 1: return memory[ip + offset] // Immediate mode
        case 2: return memory[relativeBase + memory[ip + offset]] // Relative mode
        default: fatalError("Invalid parameter mode: \(mode)")
        }
    }

    private func setParameter(at offset: Int, mode: Int, value: Int) {
        switch mode {
        case 0: memory[memory[ip + offset]] = value // Position mode
        case 2: memory[relativeBase + memory[ip + offset]] = value // Relative mode
        default: fatalError("Invalid parameter mode for writing: \(mode)")
        }
    }
}

// Read input from file
let input = try! String(contentsOfFile: "input.txt")
let program = input.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: ",").map { Int($0)! }

// Part 1: Count block tiles
func part1() {
    let computer = IntcodeComputer(program: program)
    computer.run()

    var blockCount = 0
    for i in stride(from: 0, to: computer.output.count, by: 3) {
        if computer.output[i + 2] == 2 {
            blockCount += 1
        }
    }
    print("Part 1: \(blockCount)")
}

// Part 2: Play the game
func part2() {
    var modifiedProgram = program
    modifiedProgram[0] = 2 // Play for free

    let computer = IntcodeComputer(program: modifiedProgram)
    var score = 0
    var paddleX = 0
    var ballX = 0

    while true {
        computer.run()
        if computer.output.isEmpty { break }

        for i in stride(from: 0, to: computer.output.count, by: 3) {
            let x = computer.output[i]
            let y = computer.output[i + 1]
            let tileId = computer.output[i + 2]

            if x == -1 && y == 0 {
                score = tileId
            } else if tileId == 3 {
                paddleX = x
            } else if tileId == 4 {
                ballX = x
            }
        }
        computer.output.removeAll()

        // Joystick control
        if ballX < paddleX {
            computer.input.append(-1)
        } else if ballX > paddleX {
            computer.input.append(1)
        } else {
            computer.input.append(0)
        }
    }

    print("Part 2: \(score)")
}

part1()
part2()
