import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let directions = input.components(separatedBy: ", ")

var x = 0
var y = 0
var currentDirection = 0

for direction in directions {
    let turn = direction[direction.startIndex]
    let distance = Int(direction.dropFirst())!

    if turn == "R" {
        currentDirection += 1
    } else {
        currentDirection -= 1
    }

    if currentDirection == -1 {
        currentDirection = 3
    } else if currentDirection == 4 {
        currentDirection = 0
    }

    switch currentDirection {
    case 0:
        y += distance
    case 1:
        x += distance
    case 2:
        y -= distance
    case 3:
        x -= distance
    default:
        break
    }
}

let distance = abs(x) + abs(y)
print(distance)