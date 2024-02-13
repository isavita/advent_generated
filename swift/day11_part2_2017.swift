import Foundation

let input = try String(contentsOfFile: "input.txt").components(separatedBy: ",")

var x = 0
var y = 0
var z = 0
var maxDistance = 0

for direction in input {
    switch direction {
    case "n":
        y += 1
        z -= 1
    case "ne":
        x += 1
        z -= 1
    case "se":
        x += 1
        y -= 1
    case "s":
        y -= 1
        z += 1
    case "sw":
        x -= 1
        z += 1
    case "nw":
        x -= 1
        y += 1
    default:
        break
    }
    
    let distance = (abs(x) + abs(y) + abs(z)) / 2
    if distance > maxDistance {
        maxDistance = distance
    }
}

print(maxDistance)