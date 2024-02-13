
import Foundation

func generateNextValue(previousValue: Int, factor: Int, criteria: Int) -> Int {
    var nextValue = (previousValue * factor) % 2147483647
    while nextValue % criteria != 0 {
        nextValue = (nextValue * factor) % 2147483647
    }
    return nextValue
}

let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
let lines = input.components(separatedBy: .newlines)

let startingValues = lines.map { line -> Int in
    let components = line.components(separatedBy: .whitespaces)
    return Int(components[components.count - 1])!
}

var generatorA = startingValues[0]
var generatorB = startingValues[1]

var count = 0

for _ in 1...5000000 {
    generatorA = generateNextValue(previousValue: generatorA, factor: 16807, criteria: 4)
    generatorB = generateNextValue(previousValue: generatorB, factor: 48271, criteria: 8)
    
    if generatorA & 0xFFFF == generatorB & 0xFFFF {
        count += 1
    }
}

print(count)
