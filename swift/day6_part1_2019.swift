
import Foundation

let input = try String(contentsOfFile: "input.txt")
let orbits = input.components(separatedBy: "\n").filter { !$0.isEmpty }

var orbitMap = [String: String]()

for orbit in orbits {
    let objects = orbit.components(separatedBy: ")")
    orbitMap[objects[1]] = objects[0]
}

func countOrbits(for object: String) -> Int {
    var count = 0
    var currentObject = object

    while let nextObject = orbitMap[currentObject] {
        count += 1
        currentObject = nextObject
    }

    return count
}

let totalOrbits = orbitMap.keys.reduce(0) { $0 + countOrbits(for: $1) }
print(totalOrbits)
