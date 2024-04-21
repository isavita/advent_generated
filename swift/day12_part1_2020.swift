import Foundation

struct Ship {
    var x, y, facing: Int

    mutating func processInstruction(_ action: Character, _ value: Int) {
        switch action {
        case "N":
            y += value
        case "S":
            y -= value
        case "E":
            x += value
        case "W":
            x -= value
        case "L":
            facing = (facing - value + 360) % 360
        case "R":
            facing = (facing + value) % 360
        case "F":
            switch facing {
            case 0:
                x += value
            case 90:
                y -= value
            case 180:
                x -= value
            case 270:
                y += value
            default:
                break
            }
        default:
            print("Invalid action: \(action)")
        }
    }
}

do {
    let fileURL = URL(fileURLWithPath: "input.txt")
    let content = try String(contentsOf: fileURL, encoding: .utf8)
    let lines = content.components(separatedBy: "\n")

    var ship = Ship(x: 0, y: 0, facing: 0)
    for line in lines {
        let action = line.first!
        let value = Int(String(line.dropFirst()))!
        ship.processInstruction(action, value)
    }

    let manhattanDistance = abs(ship.x) + abs(ship.y)
    print(manhattanDistance)
} catch {
    print("Error reading file:", error)
}