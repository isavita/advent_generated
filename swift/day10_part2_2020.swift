
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let lines = input.components(separatedBy: .newlines)

var adapters = [0]
for line in lines {
    if let joltage = Int(line) {
        adapters.append(joltage)
    }
}

adapters.sort()
adapters.append(adapters.last! + 3)

func countArrangements(_ adapters: [Int]) -> Int64 {
    var ways: [Int: Int64] = [0: 1]

    for i in 1..<adapters.count {
        let currentJoltage = adapters[i]
        for diff in [1, 2, 3] {
            ways[currentJoltage, default: 0] += ways[currentJoltage - diff, default: 0]
        }
    }

    return ways[adapters.last!, default: 0]
}

print(countArrangements(adapters))
