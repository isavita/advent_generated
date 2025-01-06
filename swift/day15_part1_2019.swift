
import Foundation

// Function to read the Intcode program from a file
func readIntcodeProgram(fromFile filePath: String) -> [Int] {
    guard let fileContents = try? String(contentsOfFile: filePath) else {
        fatalError("Could not read file: \(filePath)")
    }
    return fileContents.trimmingCharacters(in: .whitespacesAndNewlines)
                       .components(separatedBy: ",")
                       .compactMap { Int($0) }
}

// Intcode computer implementation
class IntcodeComputer {
    var program: [Int]
    var input: [Int] = []
    var output: [Int] = []
    var instructionPointer = 0
    var relativeBase = 0
    var isHalted = false

    init(program: [Int]) {
        self.program = program + Array(repeating: 0, count: 1000) // Extend memory
    }

    func run() {
        while !isHalted {
            let instruction = program[instructionPointer]
            let opcode = instruction % 100
            let paramModes = [
                (instruction / 100) % 10,
                (instruction / 1000) % 10,
                (instruction / 10000) % 10
            ]

            switch opcode {
            case 1: // Addition
                let param1 = getParameter(at: 1, mode: paramModes[0])
                let param2 = getParameter(at: 2, mode: paramModes[1])
                setParameter(at: 3, mode: paramModes[2], value: param1 + param2)
                instructionPointer += 4
            case 2: // Multiplication
                let param1 = getParameter(at: 1, mode: paramModes[0])
                let param2 = getParameter(at: 2, mode: paramModes[1])
                setParameter(at: 3, mode: paramModes[2], value: param1 * param2)
                instructionPointer += 4
            case 3: // Input
                if input.isEmpty { return } // Wait for input
                setParameter(at: 1, mode: paramModes[0], value: input.removeFirst())
                instructionPointer += 2
            case 4: // Output
                output.append(getParameter(at: 1, mode: paramModes[0]))
                instructionPointer += 2
            case 5: // Jump-if-true
                if getParameter(at: 1, mode: paramModes[0]) != 0 {
                    instructionPointer = getParameter(at: 2, mode: paramModes[1])
                } else {
                    instructionPointer += 3
                }
            case 6: // Jump-if-false
                if getParameter(at: 1, mode: paramModes[0]) == 0 {
                    instructionPointer = getParameter(at: 2, mode: paramModes[1])
                } else {
                    instructionPointer += 3
                }
            case 7: // Less than
                let param1 = getParameter(at: 1, mode: paramModes[0])
                let param2 = getParameter(at: 2, mode: paramModes[1])
                setParameter(at: 3, mode: paramModes[2], value: param1 < param2 ? 1 : 0)
                instructionPointer += 4
            case 8: // Equals
                let param1 = getParameter(at: 1, mode: paramModes[0])
                let param2 = getParameter(at: 2, mode: paramModes[1])
                setParameter(at: 3, mode: paramModes[2], value: param1 == param2 ? 1 : 0)
                instructionPointer += 4
            case 9: // Adjust relative base
                relativeBase += getParameter(at: 1, mode: paramModes[0])
                instructionPointer += 2
            case 99: // Halt
                isHalted = true
            default:
                fatalError("Invalid opcode: \(opcode)")
            }
        }
    }

    private func getParameter(at offset: Int, mode: Int) -> Int {
        let address: Int
        switch mode {
        case 0: // Position mode
            address = program[instructionPointer + offset]
        case 1: // Immediate mode
            address = instructionPointer + offset
        case 2: // Relative mode
            address = program[instructionPointer + offset] + relativeBase
        default:
            fatalError("Invalid parameter mode: \(mode)")
        }
        return program[address]
    }

    private func setParameter(at offset: Int, mode: Int, value: Int) {
        let address: Int
        switch mode {
        case 0: // Position mode
            address = program[instructionPointer + offset]
        case 2: // Relative mode
            address = program[instructionPointer + offset] + relativeBase
        default:
            fatalError("Invalid parameter mode for writing: \(mode)")
        }
        program[address] = value
    }
}

// Direction enum
enum Direction: Int {
    case north = 1, south = 2, west = 3, east = 4

    var opposite: Direction {
        switch self {
        case .north: return .south
        case .south: return .north
        case .west: return .east
        case .east: return .west
        }
    }
}

// Point struct
struct Point: Hashable {
    let x: Int
    let y: Int

    func moved(to direction: Direction) -> Point {
        switch direction {
        case .north: return Point(x: x, y: y - 1)
        case .south: return Point(x: x, y: y + 1)
        case .west: return Point(x: x - 1, y: y)
        case .east: return Point(x: x + 1, y: y)
        }
    }
}

// Function to explore the area and find the oxygen system
func findOxygenSystem(program: [Int]) -> Int {
    var visited: [Point: Int] = [:] // Point: steps
    var queue: [(Point, Int, IntcodeComputer)] = [(Point(x: 0, y: 0), 0, IntcodeComputer(program: program))]
    visited[Point(x: 0, y: 0)] = 0

    while !queue.isEmpty {
        let (currentPoint, steps, computer) = queue.removeFirst()

        for direction in [Direction.north, .south, .west, .east] {
            let nextPoint = currentPoint.moved(to: direction)
            if visited[nextPoint] == nil {
                let newComputer = IntcodeComputer(program: computer.program)
                newComputer.instructionPointer = computer.instructionPointer
                newComputer.relativeBase = computer.relativeBase
                newComputer.input = [direction.rawValue]
                newComputer.run()
                
                let status = newComputer.output.removeFirst()

                if status == 2 {
                    return steps + 1 // Found the oxygen system
                } else if status == 1 {
                    visited[nextPoint] = steps + 1
                    queue.append((nextPoint, steps + 1, newComputer))
                } else {
                    visited[nextPoint] = -1 // Mark as wall
                }
            }
        }
    }

    return -1 // Oxygen system not found (shouldn't happen)
}

// Main execution
let filePath = "input.txt"
let intcodeProgram = readIntcodeProgram(fromFile: filePath)
let minSteps = findOxygenSystem(program: intcodeProgram)

print("Minimum steps to reach oxygen system: \(minSteps)")
