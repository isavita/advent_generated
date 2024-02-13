
import Foundation

let input = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
let startingNumbers = input.components(separatedBy: ",").compactMap { Int($0) }

var spokenNumbers: [Int: Int] = [:]
var lastNumber = 0

for i in 0..<2020 {
    let spoken = i < startingNumbers.count ? startingNumbers[i] : spokenNumbers[lastNumber, default: 0] == 0 ? 0 : i - spokenNumbers[lastNumber]!
    spokenNumbers[lastNumber] = i
    lastNumber = spoken
}

print(lastNumber)
