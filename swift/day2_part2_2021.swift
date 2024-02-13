
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let fileContent = try String(contentsOf: fileURL)
let lines = fileContent.components(separatedBy: .newlines)

var horizontalPosition = 0
var depth = 0
var aim = 0

for line in lines {
    let command = line.components(separatedBy: " ")
    
    guard command.count >= 2 else {
        continue
    }
    
    let direction = command[0]
    let units = Int(command[1]) ?? 0

    switch direction {
    case "forward":
        horizontalPosition += units
        depth += aim * units
    case "down":
        aim += units
    case "up":
        aim -= units
    default:
        print("Invalid direction")
    }
}

let product = horizontalPosition * depth
print(product)
