
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let lines = input.components(separatedBy: .newlines)

var currentElfCalories = 0
var maxCalories = 0

for line in lines {
    if line.isEmpty {
        maxCalories = max(maxCalories, currentElfCalories)
        currentElfCalories = 0
    } else {
        currentElfCalories += Int(line) ?? 0
    }
}

print(maxCalories)
