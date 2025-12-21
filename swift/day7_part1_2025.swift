
import Foundation

let path = "input.txt"
guard let data = try? String(contentsOfFile: path, encoding: .utf8) else { exit(1) }
let grid = data.split(separator: "\n").map { String($0) }.filter { !$0.isEmpty }
guard !grid.isEmpty else { exit(0) }

let height = grid.count
let width = grid[0].count

var startX = -1, startY = -1, found = false
for (y, row) in grid.enumerated() where !found {
    for (x, ch) in row.enumerated() where ch == "S" {
        startX = x; startY = y; found = true; break
    }
}
guard found else { exit(1) }

var active: Set<Int> = [startX]
var splits = 0

for y in startY..<height {
    var next = Set<Int>()
    for x in active {
        guard x >= 0 && x < width else { continue }
        let cell = grid[y][grid[y].index(grid[y].startIndex, offsetBy: x)]
        if cell == "^" {
            splits += 1
            if x > 0 { next.insert(x - 1) }
            if x + 1 < width { next.insert(x + 1) }
        } else {
            next.insert(x)
        }
    }
    active = next
    if active.isEmpty { break }
}

print("Total times the beam is split: \(splits)")
