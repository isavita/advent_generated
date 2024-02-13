
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
var visitedHouses: Set<String> = ["0,0"]
var santaX = 0
var santaY = 0
var roboX = 0
var roboY = 0
var isSanta = true

for direction in input {
    if isSanta {
        switch direction {
        case "^":
            santaY += 1
        case "v":
            santaY -= 1
        case ">":
            santaX += 1
        case "<":
            santaX -= 1
        default:
            break
        }
        visitedHouses.insert("\(santaX),\(santaY)")
    } else {
        switch direction {
        case "^":
            roboY += 1
        case "v":
            roboY -= 1
        case ">":
            roboX += 1
        case "<":
            roboX -= 1
        default:
            break
        }
        visitedHouses.insert("\(roboX),\(roboY)")
    }
    isSanta.toggle()
}

print(visitedHouses.count)
