
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let content = try String(contentsOf: fileURL)
let lines = content.components(separatedBy: "\n")
let parts = lines[0].components(separatedBy: ", ")
let xRange = parts[0].suffix(from: parts[0].index(parts[0].startIndex, offsetBy: 15)).components(separatedBy: "..")
let yRange = parts[1].suffix(from: parts[1].index(parts[1].startIndex, offsetBy: 2)).components(separatedBy: "..")
let xMin = Int(xRange[0])!
let xMax = Int(xRange[1])!
let yMin = Int(yRange[0])!
let yMax = Int(yRange[1])!

var maxY = -1 << 30
for xVel in -1000...1000 {
    for yVel in -1000...1000 {
        var xPos = 0
        var yPos = 0
        var curXVel = xVel
        var curYVel = yVel
        var highestY = yPos
        while true {
            xPos += curXVel
            yPos += curYVel

            if xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax {
                if highestY > maxY {
                    maxY = highestY
                }
                break
            }

            if isMovingAway(xPos: xPos, yPos: yPos, xVel: curXVel, yVel: curYVel, xMin: xMin, xMax: xMax, yMin: yMin, yMax: yMax) {
                break
            }

            if curXVel > 0 {
                curXVel -= 1
            } else if curXVel < 0 {
                curXVel += 1
            }

            curYVel -= 1
            if yPos > highestY {
                highestY = yPos
            }
        }
    }
}

print(maxY)

func isMovingAway(xPos: Int, yPos: Int, xVel: Int, yVel: Int, xMin: Int, xMax: Int, yMin: Int, yMax: Int) -> Bool {
    if xPos < xMin && xVel < 0 {
        return true
    }
    if xPos > xMax && xVel > 0 {
        return true
    }
    if yPos < yMin && yVel < 0 {
        return true
    }
    return false
}
