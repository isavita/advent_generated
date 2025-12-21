
import Foundation

struct Point { let r: Int; let c: Int }

let data = try! Data(contentsOf: URL(fileURLWithPath: "input.txt"))
let lines = String(data: data, encoding: .utf8)!
    .split(separator: "\n", omittingEmptySubsequences: true)
    .map { Array($0.utf8) }

guard !lines.isEmpty else {
    print("Total rolls removed: 0")
    exit(0)
}

var grid = lines
let rows = grid.count
let cols = grid[0].count
let at: UInt8 = 64
let dot: UInt8 = 46
let offsets = [-1, 0, 1]

var totalRemoved = 0

while true {
    var toRemove = [Point]()
    for r in 0..<rows {
        for c in 0..<cols where grid[r][c] == at {
            var neighbors = 0
            for dr in offsets {
                for dc in offsets where !(dr == 0 && dc == 0) {
                    let nr = r + dr, nc = c + dc
                    if nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] == at {
                        neighbors += 1
                    }
                }
            }
            if neighbors < 4 { toRemove.append(Point(r: r, c: c)) }
        }
    }
    if toRemove.isEmpty { break }
    totalRemoved += toRemove.count
    for p in toRemove { grid[p.r][p.c] = dot }
}

print("Total rolls removed: \(totalRemoved)")
