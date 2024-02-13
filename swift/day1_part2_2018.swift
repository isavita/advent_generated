
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let changes = input.components(separatedBy: "\n").compactMap { Int($0) }

let resultingFrequency = changes.reduce(0, +)
print(resultingFrequency)

var seenFrequencies = Set<Int>()
var currentFrequency = 0
var index = 0

while true {
    currentFrequency += changes[index]
    if seenFrequencies.contains(currentFrequency) {
        print(currentFrequency)
        break
    }
    seenFrequencies.insert(currentFrequency)
    index = (index + 1) % changes.count
}
