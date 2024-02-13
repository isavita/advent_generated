
import Foundation

let input = try String(contentsOfFile: "input.txt").components(separatedBy: "\n")
let depth = Int(input[0].components(separatedBy: " ")[1])!
let target = input[1].components(separatedBy: " ")[1].components(separatedBy: ",").map { Int($0)! }

var erosionLevels = [[Int]](repeating: [Int](repeating: 0, count: target[0] + 1), count: target[1] + 1)

for y in 0...target[1] {
    for x in 0...target[0] {
        var geologicIndex = 0
        if (x == 0 && y == 0) || (x == target[0] && y == target[1]) {
            geologicIndex = 0
        } else if y == 0 {
            geologicIndex = x * 16807
        } else if x == 0 {
            geologicIndex = y * 48271
        } else {
            geologicIndex = erosionLevels[y][x - 1] * erosionLevels[y - 1][x]
        }
        erosionLevels[y][x] = (geologicIndex + depth) % 20183
    }
}

var riskLevel = 0
for y in 0...target[1] {
    for x in 0...target[0] {
        riskLevel += erosionLevels[y][x] % 3
    }
}

print(riskLevel)
