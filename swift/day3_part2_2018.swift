
import Foundation

let input = try String(contentsOfFile: "input.txt")
let lines = input.components(separatedBy: .newlines)

var fabric = Array(repeating: Array(repeating: 0, count: 1000), count: 1000)
var nonOverlappingClaims = Set<Int>()
var overlappingClaims = Set<Int>()

for line in lines {
    let parts = line.components(separatedBy: CharacterSet.decimalDigits.inverted)
    let claim = parts.compactMap { Int($0) }
    
    for i in claim[1]..<(claim[1] + claim[3]) {
        for j in claim[2]..<(claim[2] + claim[4]) {
            if fabric[i][j] == 0 {
                fabric[i][j] = claim[0]
            } else {
                overlappingClaims.insert(claim[0])
                overlappingClaims.insert(fabric[i][j])
            }
        }
    }
}

for line in lines {
    let parts = line.components(separatedBy: CharacterSet.decimalDigits.inverted)
    let claim = parts.compactMap { Int($0) }
    
    if !overlappingClaims.contains(claim[0]) {
        nonOverlappingClaims.insert(claim[0])
    }
}

print(overlappingClaims.count)
print(nonOverlappingClaims.first!)
