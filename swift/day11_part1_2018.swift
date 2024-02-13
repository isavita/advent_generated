
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let data = try String(contentsOf: fileURL)
let serial = Int(data.trimmingCharacters(in: .whitespacesAndNewlines))!

let gridSize = 300
var grid = Array(repeating: Array(repeating: 0, count: gridSize), count: gridSize)

for y in 0..<gridSize {
    for x in 0..<gridSize {
        let rackID = x + 11
        var powerLevel = rackID * (y + 1)
        powerLevel += serial
        powerLevel *= rackID
        powerLevel = (powerLevel / 100) % 10
        powerLevel -= 5
        grid[y][x] = powerLevel
    }
}

var maxPower = -1 << 31
var maxX = 0
var maxY = 0

for y in 0..<(gridSize - 2) {
    for x in 0..<(gridSize - 2) {
        var totalPower = 0
        for dy in 0..<3 {
            for dx in 0..<3 {
                totalPower += grid[y + dy][x + dx]
            }
        }
        if totalPower > maxPower {
            maxPower = totalPower
            maxX = x + 1
            maxY = y + 1
        }
    }
}

print("\(maxX),\(maxY)")
