
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let content = try String(contentsOf: fileURL)
let grid = content.split(separator: "\n").map { Array($0) }

let h = grid.count
let w = grid[0].count
var x = 0, y = 0
var dirX = 0, dirY = 0
let dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)]
var dirIdx = 0

outerLoop: for i in 0..<h {
    for j in 0..<w {
        switch grid[i][j] {
        case "^":
            x = j
            y = i
            dirIdx = 0
            (dirX, dirY) = dirs[dirIdx]
            break outerLoop
        case ">":
            x = j
            y = i
            dirIdx = 1
            (dirX, dirY) = dirs[dirIdx]
            break outerLoop
        case "v":
            x = j
            y = i
            dirIdx = 2
            (dirX, dirY) = dirs[dirIdx]
            break outerLoop
        case "<":
            x = j
            y = i
            dirIdx = 3
            (dirX, dirY) = dirs[dirIdx]
            break outerLoop
        default:
            continue
        }
    }
}

var visited = Set<[Int]>()
visited.insert([x, y])

while true {
    let nx = x + dirX
    let ny = y + dirY
    if nx < 0 || nx >= w || ny < 0 || ny >= h {
        break
    }
    if grid[ny][nx] == "#" {
        dirIdx = (dirIdx + 1) % 4
        (dirX, dirY) = dirs[dirIdx]
        continue
    }
    x = nx
    y = ny
    visited.insert([x, y])
}

print(visited.count)
