
import Foundation

let input = try String(contentsOfFile: "input.txt")
let lines = input.components(separatedBy: .newlines)
var count = 0

for line in lines {
    let sides = line.split(separator: " ").map { Int($0)! }
    let sortedSides = sides.sorted()

    if sortedSides[0] + sortedSides[1] > sortedSides[2] {
        count += 1
    }
}

print(count)
