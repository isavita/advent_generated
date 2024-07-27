
import Foundation

let inputURL = URL(fileURLWithPath: "input.txt")
let adapters = try! String(contentsOf: inputURL).split(separator: "\n").compactMap { Int($0) }.sorted()

var joltDifferences = [1: 0, 3: 1]
var previousJoltage = 0

for adapter in adapters {
    let diff = adapter - previousJoltage
    joltDifferences[diff, default: 0] += 1
    previousJoltage = adapter
}

let product = joltDifferences[1]! * joltDifferences[3]!
print(product)
