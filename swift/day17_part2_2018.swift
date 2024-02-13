
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)
let lines = input.components(separatedBy: "\n")

var ground = Array(repeating: Array(repeating: Character("."), count: 1), count: 1)
ground[0][0] = "+"

var maxX = 0
var minX = 0
var maxY = 0
var minY = 20
var xOffset = 500
var yOffset = 0

func strToInt(_ s: String) -> Int {
    return Int(s) ?? 0
}

for line in lines {
    let split = line.replacingOccurrences(of: "[=, .]+", with: " ", options: .regularExpression).components(separatedBy: " ")
    if split[0] == "x" {
        let x = strToInt(split[1]) - xOffset
        let y1 = strToInt(split[3]) - yOffset
        let y2 = strToInt(split[4]) - yOffset

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
            ground.append(Array(repeating: Character("."), count: ground[0].count))
        }
        if y1 < minY {
            minY = y1
        }
        for i in y1...y2 {
            ground[i][x - minX] = "#"
        }

    } else {
        let y = strToInt(split[1]) - yOffset
        let x1 = strToInt(split[3]) - xOffset
        let x2 = strToInt(split[4]) - xOffset

        while y > maxY {
            maxY += 1
            ground.append(Array(repeating: Character("."), count: ground[0].count))
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

while ground[1][-minX] != "|" && waterCount < roundLimit {
    var canMove = true
    var x = -minX
    var y = 1
    var tryLeft = 0
    while canMove {
        if y + 1 > maxY || ground[y + 1][x] == "|" {
            ground[y][x] = "|"
            canMove = false
            if y >= minY {
                flowCount += 1
            }
        } else if ground[y + 1][x] == "." {
            y += 1
            tryLeft = 0
        } else if ground[y + 1][x] == "#" || ground[y + 1][x] == "~" {
            if (tryLeft == 1 && ground[y][x - 1] == "|") ||
                (tryLeft == 2 && ground[y][x + 1] == "|") ||
                (ground[y][x + 1] == "|" && ground[y][x - 1] != ".") ||
                (ground[y][x + 1] != "." && ground[y][x - 1] == "|") {
                ground[y][x] = "|"
                flowCount += 1
                canMove = false
                var i = x + 1
                while ground[y][i] == "~" {
                    ground[y][i] = "|"
                    waterCount -= 1
                    flowCount += 1
                    i += 1
                }
                i = x - 1
                while ground[y][i] == "~" {
                    ground[y][i] = "|"
                    waterCount -= 1
                    flowCount += 1
                    i -= 1
                }
            } else if (tryLeft == 0 && ground[y][x - 1] == ".") ||
                (tryLeft == 1 && ground[y][x - 1] == ".") {
                x -= 1
                tryLeft = 1
            } else if (tryLeft == 0 && ground[y][x + 1] == ".") ||
                (tryLeft == 2 && ground[y][x + 1] == ".") {
                x += 1
                tryLeft = 2
            } else {
                canMove = false
                ground[y][x] = "~"
                waterCount += 1
            }
        }

    }

}
print(waterCount)
