
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let data = try String(contentsOf: fileURL)
let startingNumbers = data.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: ",")

var spoken = [Int: Int]()

var lastSpoken = 0
for (i, number) in startingNumbers.enumerated() {
    if i == startingNumbers.count - 1 {
        lastSpoken = Int(number)!
    } else {
        let num = Int(number)!
        spoken[num] = i + 1
    }
}

for turn in startingNumbers.count + 1...30000000 {
    var nextNumber = 0
    if let lastTurn = spoken[lastSpoken] {
        nextNumber = turn - 1 - lastTurn
    }
    spoken[lastSpoken] = turn - 1
    lastSpoken = nextNumber
}

print(lastSpoken)
