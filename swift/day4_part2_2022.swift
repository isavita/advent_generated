
import Foundation

let input = try String(contentsOfFile: "input.txt")
let lines = input.components(separatedBy: .newlines)

var fullyContainsCount = 0
var overlapCount = 0

for line in lines {
    let ranges = line.components(separatedBy: ",")
    let range1 = ranges[0].components(separatedBy: "-").map { Int($0)! }
    let range2 = ranges[1].components(separatedBy: "-").map { Int($0)! }

    if (range1[0] <= range2[0] && range1[1] >= range2[1]) || (range2[0] <= range1[0] && range2[1] >= range1[1]) {
        fullyContainsCount += 1
    }

    let overlapStart = max(range1[0], range2[0])
    let overlapEnd = min(range1[1], range2[1])

    if overlapStart <= overlapEnd {
        overlapCount += 1
    }
}

print(overlapCount)
