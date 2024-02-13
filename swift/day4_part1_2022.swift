
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)

let lines = input.components(separatedBy: "\n")

var count = 0

for line in lines {
    let sections = line.components(separatedBy: ",")
    let firstRange = sections[0].components(separatedBy: "-").map { Int($0)! }
    let secondRange = sections[1].components(separatedBy: "-").map { Int($0)! }

    if (firstRange[0] <= secondRange[0] && firstRange[1] >= secondRange[1]) ||
       (secondRange[0] <= firstRange[0] && secondRange[1] >= firstRange[1]) {
        count += 1
    }
}

print(count)
