
import Foundation

let input = try! String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
let lines = input.components(separatedBy: "\n")

var ground = [[Character]]()
ground.append(Array(repeating: ".", count: 1))
ground[0][0] = "+"

var maxX = 0, minX = 0, maxY = 0, minY = 20
let xOffset = 500, yOffset = 0

for line in lines {
    let split = line.split { " =,.".contains($0) }.map { String($0) }
    if split[0] == "x" {
        let x = Int(split[1])! - xOffset
        let y1 = Int(split[3])! - yOffset
        let y2 = Int(split[4])! - yOffset

        while x >= maxX {
            maxX += 1
            for j in 0..<ground.count {
                ground[j].append(".")
            }
        }
        while x <= minX {
            minX -= 1
            for j in 0..<ground.count {
                ground[j].insert(".", at: 0)
            }
        }
        while y2 > maxY {
            maxY += 1
            ground.append(Array(repeating: ".", count: ground[0].count))
        }
        if y1 < minY {
            minY = y1
        }
        for i in y1...y2 {
            ground[i][x - minX] = "#"
        }
    } else {
        let y = Int(split[1])! - yOffset
        let x1 = Int(split[3])! - xOffset
        let x2 = Int(split[4])! - xOffset

        while y > maxY {
            maxY += 1
            ground.append(Array(repeating: ".", count: ground[0].count))
        }
        while x2 >= maxX {
            maxX += 1
            for j in 0..<ground.count {
                ground[j].append(".")
            }
        }
        while x1 <= minX {
            minX -= 1
            for j in 0..<ground.count {
                ground[j].insert(".", at: 0)
            }
        }
        for i in x1...x2 {
            ground[y][i - minX] = "#"
        }
        if y < minY {
            minY = y
        }
    }
}

var waterCount = 0
var flowCount = 0
let roundLimit = 200000

var stack: [(Int, Int)] = [(1, -minX)]

while !stack.isEmpty && waterCount < roundLimit {
    let (y, x) = stack.removeLast()
    
    if y > maxY { continue }

    if ground[y][x] == "." {
        ground[y][x] = "|"
        if y >= minY {
            flowCount += 1
        }
    }
    
    if y + 1 <= maxY && ground[y + 1][x] == "." {
        stack.append((y + 1, x))
    } else if y + 1 <= maxY && (ground[y + 1][x] == "#" || ground[y + 1][x] == "~") {
        var left = x, right = x
        var leftWall = false, rightWall = false
        
        while left >= 0 && ground[y][left] != "#" && (ground[y+1][left] == "#" || ground[y+1][left] == "~") {
            left -= 1
        }
        if left >= 0 && ground[y][left] == "#" {
            leftWall = true
        }
        
        while right < ground[y].count && ground[y][right] != "#" && (ground[y+1][right] == "#" || ground[y+1][right] == "~") {
            right += 1
        }
        
        if right < ground[y].count && ground[y][right] == "#" {
            rightWall = true
        }
        
        if leftWall && rightWall {
            for i in left+1..<right {
                if ground[y][i] == "|" {
                    flowCount -= 1
                }
                ground[y][i] = "~"
                waterCount += 1
            }
            if !stack.contains(where: { $0 == (y - 1, x) }) {
                stack.append((y - 1, x))
            }
        } else {
            for i in left+1..<right {
                if ground[y][i] == "." {
                    ground[y][i] = "|"
                    if y >= minY {
                        flowCount += 1
                    }
                }
            }
            if !leftWall && !stack.contains(where: { $0 == (y, left) }) {
                stack.append((y, left))
            }
            if !rightWall && !stack.contains(where: { $0 == (y, right) }) {
                stack.append((y, right))
            }
        }
    }
}

print(flowCount + waterCount)
