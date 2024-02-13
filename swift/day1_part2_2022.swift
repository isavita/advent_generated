
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let lines = input.components(separatedBy: .newlines)

var elvesCalories: [Int] = []
var currentElfCalories = 0

for line in lines {
    if line.isEmpty {
        elvesCalories.append(currentElfCalories)
        currentElfCalories = 0
    } else {
        if let calories = Int(line) {
            currentElfCalories += calories
        }
    }
}

elvesCalories.append(currentElfCalories)

let sortedElvesCalories = elvesCalories.sorted(by: >)
let topThreeCalories = sortedElvesCalories.prefix(3)
let totalTopThreeCalories = topThreeCalories.reduce(0, +)

print(totalTopThreeCalories)
