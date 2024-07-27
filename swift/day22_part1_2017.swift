
import Foundation

struct Position: Hashable {
    var x, y: Int
}

var grid = Set<Position>()
var startX = 0, startY = 0

if let input = try? String(contentsOfFile: "input.txt") {
    let lines = input.split(separator: "\n")
    for (y, line) in lines.enumerated() {
        for (x, char) in line.enumerated() {
            if char == "#" {
                grid.insert(Position(x: x, y: y))
            }
        }
        startX = line.count / 2
        startY = lines.count / 2
    }
}

let directions = [(0, -1), (1, 0), (0, 1), (-1, 0)]
var x = startX, y = startY, dir = 0
var infectedCount = 0

for _ in 0..<10000 {
    let pos = Position(x: x, y: y)
    if grid.contains(pos) {
        dir = (dir + 1) % 4
        grid.remove(pos)
    } else {
        dir = (dir - 1 + 4) % 4
        grid.insert(pos)
        infectedCount += 1
    }
    x += directions[dir].0
    y += directions[dir].1
}

print(infectedCount)
