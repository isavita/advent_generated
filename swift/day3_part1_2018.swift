
import Foundation

let inputURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: inputURL)
let lines = input.components(separatedBy: .newlines)

var fabric = Array(repeating: Array(repeating: 0, count: 1000), count: 1000)
var overlappingInches = 0

for line in lines {
    let parts = line.components(separatedBy: CharacterSet.decimalDigits.inverted)
    let claim = parts.compactMap { Int($0) }
    
    for i in claim[1]..<(claim[1] + claim[3]) {
        for j in claim[2]..<(claim[2] + claim[4]) {
            if fabric[i][j] == 1 {
                overlappingInches += 1
            }
            fabric[i][j] += 1
        }
    }
}

print(overlappingInches)
