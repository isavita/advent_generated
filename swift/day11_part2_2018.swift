
import Foundation

let data = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
guard let serial = Int(data) else { fatalError() }

let gridSize = 300
var grid = Array(repeating: Array(repeating: 0, count: gridSize), count: gridSize)

for y in 0..<gridSize {
    for x in 0..<gridSize {
        let rackID = x + 11
        var powerLevel = rackID * (y + 1) + serial
        powerLevel *= rackID
        powerLevel = (powerLevel / 100) % 10 - 5
        grid[y][x] = powerLevel
    }
}

var maxPower = Int.min
var maxX = 0, maxY = 0, maxSize = 0

for size in 1...gridSize {
    var totalPower = Array(repeating: Array(repeating: 0, count: gridSize + 1), count: gridSize + 1)
    
    for y in 0..<gridSize {
        for x in 0..<gridSize {
            totalPower[y + 1][x + 1] = grid[y][x] + totalPower[y][x + 1] + totalPower[y + 1][x] - totalPower[y][x]
        }
    }

    for y in 0...(gridSize - size) {
        for x in 0...(gridSize - size) {
            let power = totalPower[y + size][x + size] - totalPower[y][x + size] - totalPower[y + size][x] + totalPower[y][x]
            if power > maxPower {
                maxPower = power
                maxX = x + 1
                maxY = y + 1
                maxSize = size
            }
        }
    }
}

print("\(maxX),\(maxY),\(maxSize)")
