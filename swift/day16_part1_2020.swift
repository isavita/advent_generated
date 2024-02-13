
import Foundation

let input = try String(contentsOfFile: "input.txt")
let sections = input.components(separatedBy: "\n\n")

let rules = sections[0].components(separatedBy: "\n")
let nearbyTickets = sections[2].components(separatedBy: "\n").dropFirst().map { $0.components(separatedBy: ",").compactMap { Int($0) } }

var invalidValues = [Int]()

for ticket in nearbyTickets {
    for value in ticket {
        var valid = false
        for rule in rules {
            let ranges = rule.components(separatedBy: ": ")[1].components(separatedBy: " or ").map { $0.components(separatedBy: "-") }
            let range1 = Int(ranges[0][0])!...Int(ranges[0][1])!
            let range2 = Int(ranges[1][0])!...Int(ranges[1][1])!
            if range1.contains(value) || range2.contains(value) {
                valid = true
                break
            }
        }
        if !valid {
            invalidValues.append(value)
        }
    }
}

print(invalidValues.reduce(0, +))
