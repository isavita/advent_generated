
import Foundation

let input = try String(contentsOfFile: "input.txt")
let lines = input.components(separatedBy: .newlines)

var replacements: [(String, String)] = []
var molecule = ""

for line in lines {
    if line.contains(" => ") {
        let components = line.components(separatedBy: " => ")
        replacements.append((components[0], components[1]))
    } else if !line.isEmpty {
        molecule = line
    }
}

var distinctMolecules = Set<String>()

for (from, to) in replacements {
    var range = molecule.startIndex..<molecule.endIndex
    while let matchRange = molecule.range(of: from, options: .literal, range: range) {
        let newMolecule = molecule.replacingCharacters(in: matchRange, with: to)
        distinctMolecules.insert(newMolecule)
        range = matchRange.upperBound..<molecule.endIndex
    }
}

print(distinctMolecules.count)
