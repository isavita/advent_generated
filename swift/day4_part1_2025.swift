
import Foundation

let content = try! String(contentsOfFile: "input.txt", encoding: .utf8)
let lines = content.split(separator: "\n", omittingEmptySubsequences: true).map { String($0) }
let rows = lines.count
guard rows > 0 else { exit(0) }
let cols = lines[0].count
let grid = lines.map { Array($0) }

var accessible = 0

for y in 0..<rows {
    for x in 0..<cols {
        if grid[y][x] != "@" { continue }
        var neighbors = 0
        for dy in -1...1 {
            for dx in -1...1 {
                if dy == 0 && dx == 0 { continue }
                let ny = y + dy
                let nx = x + dx
                if ny >= 0 && ny < rows && nx >= 0 && nx < cols && grid[ny][nx] == "@" {
                    neighbors += 1
                    if neighbors == 4 { break }
                }
            }
            if neighbors == 4 { break }
        }
        if neighbors < 4 { accessible += 1 }
    }
}

print("Number of accessible rolls of paper: \(accessible)")
