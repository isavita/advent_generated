
import Foundation

// MARK: - Intcode Computer

class IntcodeComputer {
    var memory: [Int]
    var ip: Int = 0
    var relativeBase: Int = 0
    var input: [Int] = []
    var output: [Int] = []
    var isHalted = false

    init(memory: [Int]) {
        self.memory = memory
        self.memory.append(contentsOf: Array(repeating: 0, count: 10000)) // Add extra memory
    }

    func run() {
        while !isHalted {
            let instruction = memory[ip]
            let opcode = instruction % 100
            let mode1 = (instruction / 100) % 10
            let mode2 = (instruction / 1000) % 10
            let mode3 = (instruction / 10000) % 10

            switch opcode {
            case 1: // Add
                let param1 = getParameter(mode: mode1, offset: 1)
                let param2 = getParameter(mode: mode2, offset: 2)
                let resultAddress = getAddress(mode: mode3, offset: 3)
                memory[resultAddress] = param1 + param2
                ip += 4
            case 2: // Multiply
                let param1 = getParameter(mode: mode1, offset: 1)
                let param2 = getParameter(mode: mode2, offset: 2)
                let resultAddress = getAddress(mode: mode3, offset: 3)
                memory[resultAddress] = param1 * param2
                ip += 4
            case 3: // Input
                if input.isEmpty {
                    return // Wait for input
                }
                let resultAddress = getAddress(mode: mode1, offset: 1)
                memory[resultAddress] = input.removeFirst()
                ip += 2
            case 4: // Output
                let param1 = getParameter(mode: mode1, offset: 1)
                output.append(param1)
                ip += 2
            case 5: // Jump-if-true
                let param1 = getParameter(mode: mode1, offset: 1)
                let param2 = getParameter(mode: mode2, offset: 2)
                ip = param1 != 0 ? param2 : ip + 3
            case 6: // Jump-if-false
                let param1 = getParameter(mode: mode1, offset: 1)
                let param2 = getParameter(mode: mode2, offset: 2)
                ip = param1 == 0 ? param2 : ip + 3
            case 7: // Less than
                let param1 = getParameter(mode: mode1, offset: 1)
                let param2 = getParameter(mode: mode2, offset: 2)
                let resultAddress = getAddress(mode: mode3, offset: 3)
                memory[resultAddress] = param1 < param2 ? 1 : 0
                ip += 4
            case 8: // Equals
                let param1 = getParameter(mode: mode1, offset: 1)
                let param2 = getParameter(mode: mode2, offset: 2)
                let resultAddress = getAddress(mode: mode3, offset: 3)
                memory[resultAddress] = param1 == param2 ? 1 : 0
                ip += 4
            case 9: // Adjust relative base
                let param1 = getParameter(mode: mode1, offset: 1)
                relativeBase += param1
                ip += 2
            case 99: // Halt
                isHalted = true
            default:
                fatalError("Invalid opcode: \(opcode)")
            }
        }
    }

    private func getParameter(mode: Int, offset: Int) -> Int {
        switch mode {
        case 0: // Position mode
            return memory[memory[ip + offset]]
        case 1: // Immediate mode
            return memory[ip + offset]
        case 2: // Relative mode
            return memory[relativeBase + memory[ip + offset]]
        default:
            fatalError("Invalid parameter mode: \(mode)")
        }
    }

    private func getAddress(mode: Int, offset: Int) -> Int {
        switch mode {
        case 0: // Position mode
            return memory[ip + offset]
        case 2: // Relative mode
            return relativeBase + memory[ip + offset]
        default:
            fatalError("Invalid address mode: \(mode)")
        }
    }
}

// MARK: - Point and Direction

struct Point: Hashable {
    let x: Int
    let y: Int

    func moved(to direction: Direction) -> Point {
        switch direction {
        case .north:
            return Point(x: x, y: y - 1)
        case .south:
            return Point(x: x, y: y + 1)
        case .west:
            return Point(x: x - 1, y: y)
        case .east:
            return Point(x: x + 1, y: y)
        }
    }
}

enum Direction: Int {
    case north = 1
    case south = 2
    case west = 3
    case east = 4

    var opposite: Direction {
        switch self {
        case .north: return .south
        case .south: return .north
        case .west: return .east
        case .east: return .west
        }
    }
}

// MARK: - Tile

enum Tile: Int {
    case wall = 0
    case empty = 1
    case oxygenSystem = 2
}

// MARK: - Main Function
func main() {
    do {
        // Read input from file
        let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let program = input.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: ",").compactMap { Int($0) }

        // Part 1: Find the oxygen system
        var map: [Point: Tile] = [:]
        var visited: Set<Point> = []
        var queue: [(Point, [Direction])] = [(Point(x: 0, y: 0), [])] // (position, path)
        map[Point(x: 0, y: 0)] = .empty
        var oxygenSystemLocation: Point?
        var oxygenSystemPath: [Direction]?

        while !queue.isEmpty {
            let (currentPosition, path) = queue.removeFirst()
            if visited.contains(currentPosition) { continue }
            visited.insert(currentPosition)
            
            for direction in [Direction.north, .south, .west, .east] {
                let newPosition = currentPosition.moved(to: direction)
                if map[newPosition] != nil { continue } // Skip if already explored
                
                let computer = IntcodeComputer(memory: program)
                computer.input = (path + [direction]).map { $0.rawValue }
                computer.run()

                guard let status = Tile(rawValue: computer.output.last!) else {
                    fatalError("Invalid status code")
                }
                
                map[newPosition] = status

                switch status {
                case .empty:
                    queue.append((newPosition, path + [direction]))
                case .oxygenSystem:
                    oxygenSystemLocation = newPosition
                    oxygenSystemPath = path + [direction]
                    queue.append((newPosition, path + [direction])) // Continue exploring for Part 2
                case .wall:
                    continue
                }
            }
        }

        guard let pathLength = oxygenSystemPath?.count else {
                fatalError("Oxygen system not found")
        }
        print("Part 1: \(pathLength)")

        // Part 2: Calculate time to fill with oxygen
         guard let start = oxygenSystemLocation else {
            fatalError("Oxygen system location missing")
        }

        var minutes = 0
        var oxygenated: Set<Point> = [start]
        var toCheck: Set<Point> = [start]
        
        while !toCheck.isEmpty {
            var nextToCheck: Set<Point> = []
            var hasUnoxygenatedEmptySpaces = false
            
            for position in toCheck {
                for direction in [Direction.north, .south, .west, .east] {
                    let adjacentPosition = position.moved(to: direction)
                    if let tile = map[adjacentPosition], (tile == .empty || tile == .oxygenSystem), !oxygenated.contains(adjacentPosition) {
                        nextToCheck.insert(adjacentPosition)
                        
                    }
                }
            }

            for point in map.keys{
                if (map[point] == Tile.empty || map[point] == Tile.oxygenSystem) && !oxygenated.contains(point) && !nextToCheck.contains(point){
                    hasUnoxygenatedEmptySpaces = true;
                }
            }

            if !nextToCheck.isEmpty {
                minutes += 1
                oxygenated.formUnion(nextToCheck)
                
            }
            if(!hasUnoxygenatedEmptySpaces){
                break;
            }

            toCheck = nextToCheck
        }
        
        print("Part 2: \(minutes)")


    } catch {
        print("Error reading input file: \(error)")
    }
}

// Call the main function to start the program
main()
