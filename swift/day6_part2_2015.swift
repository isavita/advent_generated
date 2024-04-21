import Foundation

let gridSize = 1000
var grid = [[Int]](repeating: [Int](repeating: 0, count: gridSize), count: gridSize)

do {
    let fileContent = try String(contentsOfFile: "input.txt", encoding: .utf8)
    let instructions = fileContent.components(separatedBy: "\n")
    
    for instruction in instructions {
        processInstruction(instruction: instruction)
    }
    
    print(totalBrightness())
} catch {
    print("Error reading file: \(error)")
}

func processInstruction(instruction: String) {
    let parts = instruction.components(separatedBy: " ")
    var startX, startY, endX, endY: Int
    let coordinates = parts[parts.count-3].components(separatedBy: ",").compactMap { Int($0) }
    startX = coordinates[0]
    startY = coordinates[1]
    let endCoordinates = parts[parts.count-1].components(separatedBy: ",").compactMap { Int($0) }
    endX = endCoordinates[0]
    endY = endCoordinates[1]
    
    for x in startX...endX {
        for y in startY...endY {
            switch instruction {
            case let s where s.hasPrefix("turn on"):
                grid[x][y] += 1
            case let s where s.hasPrefix("turn off"):
                grid[x][y] = max(0, grid[x][y] - 1)
            case let s where s.hasPrefix("toggle"):
                grid[x][y] += 2
            default:
                break
            }
        }
    }
}

func totalBrightness() -> Int {
    return grid.flatMap { $0 }.reduce(0, +)
}