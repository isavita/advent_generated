
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let content = try String(contentsOf: fileURL)
let lines = content.split(separator: "\n")

let width = 101
let height = 103
var robots: [[Int]] = []

for line in lines {
    let parts = line.split(separator: " ")
    let pPart = parts[0].dropFirst(2)
    let vPart = parts[1].dropFirst(2)
    let pos = pPart.split(separator: ",")
    let vel = vPart.split(separator: ",")
    let px = Int(pos[0])!
    let py = Int(pos[1])!
    let vx = Int(vel[0])!
    let vy = Int(vel[1])!
    robots.append([px, py, vx, vy])
}

for _ in 0..<100 {
    for j in 0..<robots.count {
        robots[j][0] = (robots[j][0] + robots[j][2]).mod(width)
        robots[j][1] = (robots[j][1] + robots[j][3]).mod(height)
    }
}

var q1 = 0, q2 = 0, q3 = 0, q4 = 0
for r in robots {
    let x = r[0], y = r[1]
    if x == 50 || y == 51 { continue }
    if x < 50 && y < 51 { q1 += 1 }
    else if x > 50 && y < 51 { q2 += 1 }
    else if x < 50 && y > 51 { q3 += 1 }
    else if x > 50 && y > 51 { q4 += 1 }
}

print(q1 * q2 * q3 * q4)

extension BinaryInteger {
    func mod(_ other: Self) -> Self {
        let r = self % other
        return r >= 0 ? r : r + other
    }
}
