import Foundation

let file = "input.txt"
var input = ""
do {
    input = try String(contentsOfFile: file)
} catch {
    print(error)
}

var x = 0
var y = 0
var visitedHouses: Set<String> = ["0,0"]

for char in input {
    switch char {
    case "^":
        y += 1
    case "v":
        y -= 1
    case ">":
        x += 1
    case "<":
        x -= 1
    default:
        break
    }
    visitedHouses.insert("\(x),\(y)")
}

print(visitedHouses.count)