
import Foundation

enum TurnDirection: Int {
    case left = 0
    case right = 1
}

enum PaintColor: Int {
    case black = 0
    case white = 1
}

enum Direction {
    case up, down, left, right
}

func runIntcode(program: [Int], input: () -> Int, output: (Int) -> Void) {
    var memory = program
    memory.reserveCapacity(program.count * 10) // prevent out of bounds
    for _ in 0..<(program.count * 9) {
        memory.append(0)
    }

    var ip = 0 // Instruction pointer
    var relativeBase = 0

    func getParameter(mode: Int, offset: Int) -> Int {
        let parameter = memory[ip + offset]
        switch mode {
        case 0: // Position mode
            return memory[parameter]
        case 1: // Immediate mode
            return parameter
        case 2: // Relative mode
            return memory[relativeBase + parameter]
        default:
            fatalError("Invalid parameter mode: \(mode)")
        }
    }

    func setParameter(mode: Int, offset: Int, value: Int) {
        let parameter = memory[ip + offset]
        switch mode {
        case 0: // Position mode
            memory[parameter] = value
        case 2: // Relative mode
            memory[relativeBase + parameter] = value
        default:
            fatalError("Invalid parameter mode for writing: \(mode)")
        }
    }


    while memory[ip] != 99 {
        let instruction = String(format: "%05d", memory[ip])
        let opcode = Int(instruction.suffix(2))!
        let mode1 = Int(String(instruction[instruction.index(instruction.startIndex, offsetBy: 2)]))!
        let mode2 = Int(String(instruction[instruction.index(instruction.startIndex, offsetBy: 1)]))!
        let mode3 = Int(String(instruction[instruction.index(instruction.startIndex, offsetBy: 0)]))!
        
        switch opcode {
        case 1: // Addition
            let val1 = getParameter(mode: mode1, offset: 1)
            let val2 = getParameter(mode: mode2, offset: 2)
            setParameter(mode: mode3, offset: 3, value: val1 + val2)
            ip += 4
        case 2: // Multiplication
            let val1 = getParameter(mode: mode1, offset: 1)
            let val2 = getParameter(mode: mode2, offset: 2)
            setParameter(mode: mode3, offset: 3, value: val1 * val2)
            ip += 4
        case 3: // Input
            let inputValue = input()
            setParameter(mode: mode1, offset: 1, value: inputValue)
            ip += 2
        case 4: // Output
            let val = getParameter(mode: mode1, offset: 1)
            output(val)
            ip += 2
        case 5: // Jump-if-true
            if getParameter(mode: mode1, offset: 1) != 0 {
                ip = getParameter(mode: mode2, offset: 2)
            } else {
                ip += 3
            }
        case 6: // Jump-if-false
            if getParameter(mode: mode1, offset: 1) == 0 {
                ip = getParameter(mode: mode2, offset: 2)
            } else {
                ip += 3
            }
        case 7: // Less than
            let val1 = getParameter(mode: mode1, offset: 1)
            let val2 = getParameter(mode: mode2, offset: 2)
            setParameter(mode: mode3, offset: 3, value: val1 < val2 ? 1 : 0)
            ip += 4
        case 8: // Equals
            let val1 = getParameter(mode: mode1, offset: 1)
            let val2 = getParameter(mode: mode2, offset: 2)
            setParameter(mode: mode3, offset: 3, value: val1 == val2 ? 1 : 0)
            ip += 4
        case 9: // Adjust relative base
            relativeBase += getParameter(mode: mode1, offset: 1)
            ip += 2
        default:
            fatalError("Unknown opcode: \(opcode)")
        }
    }
}


func solvePart1(program: [Int]) -> Int {
    var paintedPanels: [String: Int] = [:]
    var currentPosition = (x: 0, y: 0)
    var currentDirection = Direction.up
    
    var inputClosure: () -> Int = {
        let panelKey = "\(currentPosition.x),\(currentPosition.y)"
        return paintedPanels[panelKey, default: 0]
    }
    
    var outputCount = 0
    var paintColor: Int = 0
    
    var outputClosure: (Int) -> Void = { outputValue in
        if outputCount % 2 == 0 {
            paintColor = outputValue
        } else {
            let turnDirection = TurnDirection(rawValue: outputValue)!
            let panelKey = "\(currentPosition.x),\(currentPosition.y)"
            paintedPanels[panelKey] = paintColor
            
            switch currentDirection {
            case .up:
                currentDirection = turnDirection == .left ? .left : .right
            case .down:
                currentDirection = turnDirection == .left ? .right : .left
            case .left:
                currentDirection = turnDirection == .left ? .down : .up
            case .right:
                currentDirection = turnDirection == .left ? .up : .down
            }
            
            switch currentDirection {
            case .up:
                currentPosition.y += 1
            case .down:
                currentPosition.y -= 1
            case .left:
                currentPosition.x -= 1
            case .right:
                currentPosition.x += 1
            }
        }
        outputCount += 1
    }
    
    runIntcode(program: program, input: inputClosure, output: outputClosure)
    
    return paintedPanels.count
}

func solvePart2(program: [Int]) -> String {
    var paintedPanels: [String: Int] = [:]
    var currentPosition = (x: 0, y: 0)
    var currentDirection = Direction.up
    
    paintedPanels["0,0"] = 1 // Start on a white panel
    
    var inputClosure: () -> Int = {
        let panelKey = "\(currentPosition.x),\(currentPosition.y)"
        return paintedPanels[panelKey, default: 0]
    }
    
    var outputCount = 0
    var paintColor: Int = 0
    
    var outputClosure: (Int) -> Void = { outputValue in
        if outputCount % 2 == 0 {
            paintColor = outputValue
        } else {
            let turnDirection = TurnDirection(rawValue: outputValue)!
            let panelKey = "\(currentPosition.x),\(currentPosition.y)"
            paintedPanels[panelKey] = paintColor
            
            switch currentDirection {
            case .up:
                currentDirection = turnDirection == .left ? .left : .right
            case .down:
                currentDirection = turnDirection == .left ? .right : .left
            case .left:
                currentDirection = turnDirection == .left ? .down : .up
            case .right:
                currentDirection = turnDirection == .left ? .up : .down
            }
            
            switch currentDirection {
            case .up:
                currentPosition.y += 1
            case .down:
                currentPosition.y -= 1
            case .left:
                currentPosition.x -= 1
            case .right:
                currentPosition.x += 1
            }
        }
        outputCount += 1
    }
    
    runIntcode(program: program, input: inputClosure, output: outputClosure)
    
    let minX = paintedPanels.keys.compactMap { Int($0.split(separator: ",")[0]) }.min()!
    let maxX = paintedPanels.keys.compactMap { Int($0.split(separator: ",")[0]) }.max()!
    let minY = paintedPanels.keys.compactMap { Int($0.split(separator: ",")[1]) }.min()!
    let maxY = paintedPanels.keys.compactMap { Int($0.split(separator: ",")[1]) }.max()!

    var outputString = ""
    for y in (minY...maxY).reversed() {
        for x in minX...maxX {
            let panelKey = "\(x),\(y)"
            let color = paintedPanels[panelKey, default: 0]
            outputString += color == 1 ? "#" : " "
        }
        outputString += "\n"
    }

    return outputString
}

func main() {
    do {
        let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let program = input.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: ",").compactMap { Int($0) }

        let part1Result = solvePart1(program: program)
        print("Part 1: \(part1Result)")
        
        let part2Result = solvePart2(program: program)
        print("Part 2:\n\(part2Result)")

    } catch {
        print("Error reading file: \(error)")
    }
}

main()

