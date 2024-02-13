
import Foundation

struct Point: Hashable {
    var x: Int
    var y: Int
}

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)
let lines = input.components(separatedBy: "\n")
let wire1 = getPointsWithSteps(path: lines[0])
let wire2 = getPointsWithSteps(path: lines[1])

var minSteps = Int.max
for (point, steps1) in wire1 {
    if let steps2 = wire2[point] {
        let totalSteps = steps1 + steps2
        if totalSteps < minSteps {
            minSteps = totalSteps
        }
    }
}

print(minSteps)

func getPointsWithSteps(path: String) -> [Point: Int] {
    var points = [Point: Int]()
    var current = Point(x: 0, y: 0)
    var steps = 0
    for move in path.components(separatedBy: ",") {
        let dir = move.first!
        let dist = Int(move.dropFirst())!
        for _ in 0..<dist {
            steps += 1
            switch dir {
            case "U":
                current.y += 1
            case "D":
                current.y -= 1
            case "L":
                current.x -= 1
            case "R":
                current.x += 1
            default:
                break
            }
            if points[current] == nil {
                points[current] = steps
            }
        }
    }
    return points
}
