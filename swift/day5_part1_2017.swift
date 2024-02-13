
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
var offsets = input.components(separatedBy: "\n").compactMap { Int($0) }

var steps = 0
var currentIndex = 0

while currentIndex >= 0 && currentIndex < offsets.count {
    let offset = offsets[currentIndex]
    offsets[currentIndex] += 1
    currentIndex += offset
    steps += 1
}

print(steps)
