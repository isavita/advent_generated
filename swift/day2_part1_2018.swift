
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)
let boxIDs = input.components(separatedBy: "\n")

var twos = 0
var threes = 0

for boxID in boxIDs {
    var counts: [Character: Int] = [:]
    for char in boxID {
        counts[char, default: 0] += 1
    }
    if counts.values.contains(2) {
        twos += 1
    }
    if counts.values.contains(3) {
        threes += 1
    }
}

print(twos * threes)
