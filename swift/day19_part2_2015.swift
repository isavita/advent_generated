
import Foundation

let input = try String(contentsOfFile: "input.txt")
let lines = input.components(separatedBy: .newlines)

var replacements: [(String, String)] = []
var molecule = ""

for line in lines {
    if line.contains("=>") {
        let components = line.components(separatedBy: " => ")
        replacements.append((components[0], components[1]))
    } else if !line.isEmpty {
        molecule = line
    }
}

func partOne() {
    var molecules = Set<String>()

    for (from, to) in replacements {
        var range = molecule.startIndex..<molecule.endIndex

        while let foundRange = molecule.range(of: from, options: .literal, range: range) {
            let newMolecule = molecule.replacingCharacters(in: foundRange, with: to)
            molecules.insert(newMolecule)
            range = foundRange.upperBound..<molecule.endIndex
        }
    }

    print(molecules.count)
}

func partTwo() {
    var steps = 0
    var currentMolecule = molecule

    while currentMolecule != "e" {
        for (from, to) in replacements {
            if let range = currentMolecule.range(of: to) {
                currentMolecule.replaceSubrange(range, with: from)
                steps += 1
            }
        }
    }

    print(steps)
}

partOne()
partTwo()
