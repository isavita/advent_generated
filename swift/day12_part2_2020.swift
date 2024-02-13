
import Foundation

let input = try String(contentsOfFile: "input.txt")
let instructions = input.components(separatedBy: "\n")

var x = 0
var y = 0
var waypointX = 10
var waypointY = 1

for instruction in instructions {
    let action = instruction.prefix(1)
    let value = Int(instruction.dropFirst())!

    switch action {
    case "N":
        waypointY += value
    case "S":
        waypointY -= value
    case "E":
        waypointX += value
    case "W":
        waypointX -= value
    case "L":
        for _ in 0..<(value / 90) {
            let temp = waypointX
            waypointX = -waypointY
            waypointY = temp
        }
    case "R":
        for _ in 0..<(value / 90) {
            let temp = waypointX
            waypointX = waypointY
            waypointY = -temp
        }
    case "F":
        x += value * waypointX
        y += value * waypointY
    default:
        break
    }
}

print(abs(x) + abs(y))
