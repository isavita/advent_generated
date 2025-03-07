
import Foundation

enum ParamMode: Int {
    case position = 0
    case immediate = 1
    case relative = 2
}

class IntcodeComputer {
    var memory: [Int: Int]
    var pointer: Int
    var relativeBase: Int
    var inputs: [Int]
    var outputs: [Int]
    var halted: Bool

    init(memory: [Int]) {
        self.memory = [:]
        for (i, val) in memory.enumerated() {
            self.memory[i] = val
        }
        self.pointer = 0
        self.relativeBase = 0
        self.inputs = []
        self.outputs = []
        self.halted = false
    }

    func getParam(mode: ParamMode, param: Int) -> Int {
        switch mode {
        case .position:
            return memory[param, default: 0]
        case .immediate:
            return param
        case .relative:
            return memory[relativeBase + param, default: 0]
        }
    }

    func setParam(mode: ParamMode, param: Int, value: Int) {
        switch mode {
        case .position:
            memory[param] = value
        case .relative:
            memory[relativeBase + param] = value
        default:
            fatalError("Unknown parameter mode for writing: \(mode)")
        }
    }

    func run() {
        while true {
            let instruction = memory[pointer, default: 0]
            let opcode = instruction % 100
            let modes: [ParamMode] = [
                ParamMode(rawValue: (instruction / 100) % 10)!,
                ParamMode(rawValue: (instruction / 1000) % 10)!,
                ParamMode(rawValue: (instruction / 10000) % 10)!
            ]

            switch opcode {
            case 1:
                let param1 = memory[pointer + 1, default: 0]
                let param2 = memory[pointer + 2, default: 0]
                let param3 = memory[pointer + 3, default: 0]
                let val1 = getParam(mode: modes[0], param: param1)
                let val2 = getParam(mode: modes[1], param: param2)
                setParam(mode: modes[2], param: param3, value: val1 + val2)
                pointer += 4
            case 2:
                let param1 = memory[pointer + 1, default: 0]
                let param2 = memory[pointer + 2, default: 0]
                let param3 = memory[pointer + 3, default: 0]
                let val1 = getParam(mode: modes[0], param: param1)
                let val2 = getParam(mode: modes[1], param: param2)
                setParam(mode: modes[2], param: param3, value: val1 * val2)
                pointer += 4
            case 3:
                if inputs.isEmpty {
                    return
                }
                let param1 = memory[pointer + 1, default: 0]
                let input = inputs.removeFirst()
                setParam(mode: modes[0], param: param1, value: input)
                pointer += 2
            case 4:
                let param1 = memory[pointer + 1, default: 0]
                let output = getParam(mode: modes[0], param: param1)
                outputs.append(output)
                pointer += 2
            case 5:
                let param1 = memory[pointer + 1, default: 0]
                let param2 = memory[pointer + 2, default: 0]
                let val1 = getParam(mode: modes[0], param: param1)
                let val2 = getParam(mode: modes[1], param: param2)
                pointer = val1 != 0 ? val2 : pointer + 3
            case 6:
                let param1 = memory[pointer + 1, default: 0]
                let param2 = memory[pointer + 2, default: 0]
                let val1 = getParam(mode: modes[0], param: param1)
                let val2 = getParam(mode: modes[1], param: param2)
                pointer = val1 == 0 ? val2 : pointer + 3
            case 7:
                let param1 = memory[pointer + 1, default: 0]
                let param2 = memory[pointer + 2, default: 0]
                let param3 = memory[pointer + 3, default: 0]
                let val1 = getParam(mode: modes[0], param: param1)
                let val2 = getParam(mode: modes[1], param: param2)
                setParam(mode: modes[2], param: param3, value: val1 < val2 ? 1 : 0)
                pointer += 4
            case 8:
                let param1 = memory[pointer + 1, default: 0]
                let param2 = memory[pointer + 2, default: 0]
                let param3 = memory[pointer + 3, default: 0]
                let val1 = getParam(mode: modes[0], param: param1)
                let val2 = getParam(mode: modes[1], param: param2)
                setParam(mode: modes[2], param: param3, value: val1 == val2 ? 1 : 0)
                pointer += 4
            case 9:
                let param1 = memory[pointer + 1, default: 0]
                let val1 = getParam(mode: modes[0], param: param1)
                relativeBase += val1
                pointer += 2
            case 99:
                halted = true
                return
            default:
                fatalError("Unknown opcode: \(opcode)")
            }
        }
    }
}

func readInput(filename: String) -> [Int] {
    do {
        let content = try String(contentsOfFile: filename).trimmingCharacters(in: .whitespacesAndNewlines)
        return content.split(separator: ",").compactMap { Int($0) }
    } catch {
        fatalError("Could not read from file: \(filename), error: \(error)")
    }
}

func parseMap(output: [Int]) -> [[Character]] {
    var grid: [[Character]] = []
    var line: [Character] = []
    for c in output {
        if c == 10 {
            if !line.isEmpty {
                grid.append(line)
                line = []
            }
        } else {
            line.append(Character(UnicodeScalar(c)!))
        }
    }
    if !line.isEmpty {
        grid.append(line)
    }
    return grid
}

func findIntersections(grid: [[Character]]) -> [(Int, Int)] {
    var intersections: [(Int, Int)] = []
    for y in 1..<grid.count - 1 {
        for x in 1..<grid[0].count - 1 {
            if grid[y][x] == "#" &&
                grid[y-1][x] == "#" &&
                grid[y+1][x] == "#" &&
                grid[y][x-1] == "#" &&
                grid[y][x+1] == "#" {
                intersections.append((x, y))
            }
        }
    }
    return intersections
}

func findRobotPosition(grid: [[Character]]) -> (Int, Int, Character)? {
    for (y, row) in grid.enumerated() {
        for (x, cell) in row.enumerated() {
            if ["^", "v", "<", ">", "X"].contains(cell) {
                return (x, y, cell)
            }
        }
    }
    return nil
}

func turnLeft(direction: Character) -> Character {
    switch direction {
    case "^": return "<"
    case "<": return "v"
    case "v": return ">"
    case ">": return "^"
    default: fatalError("Unknown direction: \(direction)")
    }
}

func turnRight(direction: Character) -> Character {
    switch direction {
    case "^": return ">"
    case ">": return "v"
    case "v": return "<"
    case "<": return "^"
    default: fatalError("Unknown direction: \(direction)")
    }
}

func moveForward(x: Int, y: Int, direction: Character) -> (Int, Int) {
    switch direction {
    case "^": return (x, y - 1)
    case "v": return (x, y + 1)
    case "<": return (x - 1, y)
    case ">": return (x + 1, y)
    default: fatalError("Unknown direction: \(direction)")
    }
}

func getMovementPath(grid: [[Character]], startX: Int, startY: Int, startDir: Character) -> [String] {
    var x = startX, y = startY, direction = startDir
    var path: [String] = []
    var steps = 0

    while true {
        let (nextX, nextY) = moveForward(x: x, y: y, direction: direction)
        if nextY >= 0 && nextY < grid.count && nextX >= 0 && nextX < grid[0].count && grid[nextY][nextX] == "#" {
            x = nextX
            y = nextY
            steps += 1
        } else {
            if steps > 0 {
                path.append("\(steps)")
                steps = 0
            }

            let leftDir = turnLeft(direction: direction)
            let (nextXLeft, nextYLeft) = moveForward(x: x, y: y, direction: leftDir)
            if nextYLeft >= 0 && nextYLeft < grid.count && nextXLeft >= 0 && nextXLeft < grid[0].count && grid[nextYLeft][nextXLeft] == "#" {
                path.append("L")
                direction = leftDir
                continue
            }

            let rightDir = turnRight(direction: direction)
            let (nextXRight, nextYRight) = moveForward(x: x, y: y, direction: rightDir)
            if nextYRight >= 0 && nextYRight < grid.count && nextXRight >= 0 && nextXRight < grid[0].count && grid[nextYRight][nextXRight] == "#" {
                path.append("R")
                direction = rightDir
                continue
            }
            break
        }
    }
    return path
}

func compressMovement(path: [String]) -> (String, String, String, String)? {
    func isValidRoutine(routine: String) -> Bool {
        return routine.count <= 20
    }

    func replaceSequence(seq: [String], pattern: [String], replacement: String) -> [String] {
        var i = 0
        var res: [String] = []
        while i < seq.count {
            if Array(seq[i..<min(i+pattern.count, seq.count)]) == pattern {
                res.append(replacement)
                i += pattern.count
            } else {
                res.append(seq[i])
                i += 1
            }
        }
        return res
    }
    
    let pathStr = path.joined(separator: ",")
    let tokens = pathStr.split(separator: ",").map { String($0) }
    
    let maxFunctionLength = 20
    let maxPatternLength = 10
    
    for aLen in 1...maxPatternLength {
        let aPattern = Array(tokens[0..<aLen])
        let aStr = aPattern.joined(separator: ",")
        if aStr.count > maxFunctionLength {
            continue
        }
        let tokensAfterA = replaceSequence(seq: tokens, pattern: aPattern, replacement: "A")
        
        for bStart in aLen..<tokens.count {
            for bLen in 1...maxPatternLength {
                let bEnd = bStart + bLen
                if bEnd > tokens.count { continue }
                let bPattern = Array(tokens[bStart..<bEnd])
                let bStr = bPattern.joined(separator: ",")
                if bStr.count > maxFunctionLength {
                    continue
                }
                let tokensAfterB = replaceSequence(seq: tokensAfterA, pattern: bPattern, replacement: "B")
                
                for cStart in bEnd..<tokens.count {
                    for cLen in 1...maxPatternLength {
                        let cEnd = cStart + cLen
                        if cEnd > tokens.count { continue }
                        let cPattern = Array(tokens[cStart..<cEnd])
                        let cStr = cPattern.joined(separator: ",")
                        if cStr.count > maxFunctionLength {
                            continue
                        }
                        let tokensAfterC = replaceSequence(seq: tokensAfterB, pattern: cPattern, replacement: "C")
                        
                        var mainTokens = tokensAfterC
                        var changed = true
                        while changed {
                            changed = false
                            var tempTokens = mainTokens
                            mainTokens = []
                            var i = 0
                            while i < tempTokens.count {
                                if Array(tempTokens[i..<min(i+aPattern.count, tempTokens.count)]) == aPattern {
                                    mainTokens.append("A")
                                    i += aPattern.count
                                    changed = true
                                } else if Array(tempTokens[i..<min(i+bPattern.count, tempTokens.count)]) == bPattern {
                                    mainTokens.append("B")
                                    i += bPattern.count
                                    changed = true
                                } else if Array(tempTokens[i..<min(i+cPattern.count, tempTokens.count)]) == cPattern {
                                    mainTokens.append("C")
                                    i += cPattern.count
                                    changed = true
                                } else {
                                    mainTokens.append(tempTokens[i])
                                    i += 1
                                }
                            }
                        }
                        
                        let mainRoutine = mainTokens.joined(separator: ",")
                        if mainRoutine.allSatisfy({ "ABC,".contains($0) }) && mainRoutine.count <= 20 {
                            let functionA = aPattern.joined(separator: ",")
                            let functionB = bPattern.joined(separator: ",")
                            let functionC = cPattern.joined(separator: ",")
                            
                            if functionA.count <= 20 && functionB.count <= 20 && functionC.count <= 20 {
                                return (mainRoutine, functionA, functionB, functionC)
                            }
                        }
                    }
                }
            }
        }
    }
    
    return nil
}

func main() {
    let program = readInput(filename: "input.txt")

    var computer = IntcodeComputer(memory: program)
    computer.run()
    let output = computer.outputs
    let grid = parseMap(output: output)
    let intersections = findIntersections(grid: grid)
    let alignmentSum = intersections.reduce(0) { $0 + ($1.0 * $1.1) }
    print("Part One: Sum of alignment parameters = \(alignmentSum)")

    var programPart2 = program
    programPart2[0] = 2
    var computerPart2 = IntcodeComputer(memory: programPart2)

    guard let robot = findRobotPosition(grid: grid) else {
        fatalError("Robot not found on the scaffold.")
    }
    let (startX, startY, startDir) = robot

    let movementPath = getMovementPath(grid: grid, startX: startX, startY: startY, startDir: startDir)

    guard let (mainRoutine, functionA, functionB, functionC) = compressMovement(path: movementPath) else {
        fatalError("Error in compressing path")
    }

    let inputLines = [
        mainRoutine,
        functionA,
        functionB,
        functionC,
        "n"
    ]

    var movementInputs: [Int] = []
    for line in inputLines {
        for char in line {
            movementInputs.append(Int(char.asciiValue!))
        }
        movementInputs.append(10)
    }

    computerPart2.inputs = movementInputs

    while !computerPart2.halted {
        computerPart2.run()
    }

    let dustCollected = computerPart2.outputs.last!
    print("Part Two: Dust collected = \(dustCollected)")
}

main()
