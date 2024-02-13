
import Foundation

let input = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)

func react(polymer: String) -> String {
    var stack: [Character] = []

    for unit in polymer {
        if let lastUnit = stack.last, lastUnit != unit && lastUnit.uppercased() == String(unit).uppercased() {
            stack.removeLast()
        } else {
            stack.append(unit)
        }
    }

    return String(stack)
}

let reactedPolymer = react(polymer: input)
print(reactedPolymer.count)

let units = Set(input.uppercased())
var shortestPolymerLength = Int.max

for unit in units {
    let filteredPolymer = input.filter { $0.uppercased() != String(unit) }
    let reactedFilteredPolymer = react(polymer: filteredPolymer)
    shortestPolymerLength = min(shortestPolymerLength, reactedFilteredPolymer.count)
}

print(shortestPolymerLength)
