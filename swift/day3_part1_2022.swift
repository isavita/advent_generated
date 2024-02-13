
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let contents = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)

var sum = 0
for line in contents.components(separatedBy: "\n") {
    let firstCompartment = line.prefix(line.count / 2)
    let secondCompartment = line.suffix(line.count / 2)
    
    let commonItems = Set(firstCompartment).intersection(Set(secondCompartment))
    for item in commonItems {
        let asciiValue = item.asciiValue ?? 0
        let priority = asciiValue >= 97 ? Int(asciiValue - 96) : Int(asciiValue - 38)
        sum += priority
    }
}

print(sum)
