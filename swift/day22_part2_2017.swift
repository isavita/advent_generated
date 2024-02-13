
import Foundation

struct Position: Hashable {
    var x: Int
    var y: Int
}

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let lines = input.components(separatedBy: .newlines)

var grid = [Position: Int]()
var startX = 0
var startY = 0

for y in 0..<lines.count {
    let line = lines[y]
    for x in 0..<line.count {
        let c = line[line.index(line.startIndex, offsetBy: x)]
        if c == "#" {
            grid[Position(x: x, y: y)] = 2
        }
    }
    startX = line.count / 2
    startY = y / 2
}

let dx = [0, 1, 0, -1]
let dy = [-1, 0, 1, 0]

var x = startX
var y = startY
var dir = 0
var infectedCount = 0

for _ in 0..<10000000 {
    let pos = Position(x: x, y: y)
    switch grid[pos, default: 0] {
    case 0:
        dir = (dir - 1 + 4) % 4
        grid[pos] = 1
    case 1:
        grid[pos] = 2
        infectedCount += 1
    case 2:
        dir = (dir + 1) % 4
        grid[pos] = 3
    case 3:
        dir = (dir + 2) % 4
        grid[pos] = 0
    default:
        break // Added default case to make switch exhaustive
    }
    x += dx[dir]
    y += dy[dir]
}

print(infectedCount)
