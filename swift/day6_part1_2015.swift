import Foundation

let gridSize = 1000

var grid = [[Bool]](repeating: [Bool](repeating: false, count: gridSize), count: gridSize)

do {
    let fileContent = try String(contentsOfFile: "input.txt", encoding: .utf8)
    let instructions = fileContent.components(separatedBy: "\n")

    for instruction in instructions {
        processInstruction(instruction: instruction)
    }
} catch {
    print("Error reading file: \(error)")
}

print(countLights())

func processInstruction(instruction: String) {
    let parts = instruction.components(separatedBy: " ")
    var startX, startY, endX, endY: Int
    let coordinates = parts[parts.count - 3].components(separatedBy: ",")
    startX = Int(coordinates[0])!
    startY = Int(coordinates[1])!
    let coordinates2 = parts[parts.count - 1].components(separatedBy: ",")
    endX = Int(coordinates2[0])!
    endY = Int(coordinates2[1])!

    for x in startX...endX {
        for y in startY...endY {
            switch instruction {
            case let s where s.hasPrefix("turn on"):
                grid[x][y] = true
            case let s where s.hasPrefix("turn off"):
                grid[x][y] = false
            case let s where s.hasPrefix("toggle"):
                grid[x][y].toggle()
            default:
                break
            }
        }
    }
}

func countLights() -> Int {
    return grid.flatMap { $0 }.filter { $0 }.count
}