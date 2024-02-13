
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let directions = input.components(separatedBy: ", ")

var x = 0
var y = 0
var currentDirection = 0
var visited: Set<String> = ["0,0"]
var firstLocationVisitedTwice: String?

for dir in directions {
    let turn = dir[dir.startIndex]
    let distance = Int(dir.dropFirst())!

    if turn == "R" {
        currentDirection = (currentDirection + 1) % 4
    } else {
        currentDirection = (currentDirection + 3) % 4
    }

    for _ in 0..<distance {
        switch currentDirection {
        case 0:
            y += 1
        case 1:
            x += 1
        case 2:
            y -= 1
        case 3:
            x -= 1
        default:
            break
        }

        let location = "\(x),\(y)"
        if firstLocationVisitedTwice == nil && visited.contains(location) {
            firstLocationVisitedTwice = location
        }
        visited.insert(location)
    }
}

let distance = abs(x) + abs(y)
print(distance)

if let firstLocation = firstLocationVisitedTwice {
    let coordinates = firstLocation.components(separatedBy: ",").compactMap { Int($0) }
    let distanceToFirstLocation = abs(coordinates[0]) + abs(coordinates[1])
    print(distanceToFirstLocation)
}
