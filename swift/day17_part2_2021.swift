
import Foundation

let input = try String(contentsOfFile: "input.txt")
let parts = input.split(separator: ", ")
let xRange = parts[0].dropFirst(15).split(separator: "..").map { Int($0)! }
let yRange = parts[1].dropFirst(2).split(separator: "..").map { Int($0)! }

let xMin = xRange[0], xMax = xRange[1]
let yMin = yRange[0], yMax = yRange[1]

var velocities = Set<String>()

for xVel in -1000...1000 {
    for yVel in -1000...1000 {
        var (xPos, yPos) = (0, 0)
        var (curXVel, curYVel) = (xVel, yVel)
        
        while true {
            xPos += curXVel
            yPos += curYVel
            
            if xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax {
                velocities.insert("\(xVel),\(yVel)")
                break
            }
            
            if (xPos < xMin && curXVel <= 0) || (xPos > xMax && curXVel >= 0) || (yPos < yMin && curYVel <= 0) {
                break
            }
            
            if curXVel != 0 {
                curXVel += (curXVel > 0) ? -1 : 1
            }
            curYVel -= 1
        }
    }
}

print(velocities.count)
