import Foundation

let input = try String(contentsOfFile: "input.txt")
var floor = 0
var position = 0

for char in input {
    position += 1
    if char == "(" {
        floor += 1
    } else if char == ")" {
        floor -= 1
    }
    
    if floor == -1 {
        break
    }
}

print(position)