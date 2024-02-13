
import Foundation

let input = try String(contentsOfFile: "input.txt")
let coordinates = input.components(separatedBy: "\n").map { $0.components(separatedBy: ", ").map { Int($0)! } }

let maxX = coordinates.map { $0[0] }.max()!
let maxY = coordinates.map { $0[1] }.max()!

func manhattanDistance(_ x1: Int, _ y1: Int, _ x2: Int, _ y2: Int) -> Int {
    return abs(x1 - x2) + abs(y1 - y2)
}

var areaSizes = Array(repeating: 0, count: coordinates.count)
var totalRegionSize = 0

for x in 0...maxX {
    for y in 0...maxY {
        var totalDistance = 0

        for (index, coord) in coordinates.enumerated() {
            totalDistance += manhattanDistance(x, y, coord[0], coord[1])
        }

        if totalDistance < 10000 {
            totalRegionSize += 1
        }
    }
}

print(totalRegionSize)
