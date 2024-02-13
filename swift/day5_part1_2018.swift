
import Foundation

let input = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)

func reactPolymer(_ polymer: String) -> String {
    var stack: [Character] = []

    for unit in polymer {
        if let lastUnit = stack.last, lastUnit.isUppercase != unit.isUppercase, lastUnit.lowercased() == unit.lowercased() {
            stack.removeLast()
        } else {
            stack.append(unit)
        }
    }

    return String(stack)
}

let reactedPolymer = reactPolymer(input)
print(reactedPolymer.count)
