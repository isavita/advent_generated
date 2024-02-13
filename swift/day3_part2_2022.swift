
import Foundation

let input = try String(contentsOfFile: "input.txt")
let lines = input.components(separatedBy: "\n")

var sum = 0

for i in stride(from: 0, to: lines.count, by: 3) {
    var commonItems: Set<Character> = Set(lines[i])
    commonItems.formIntersection(lines[i + 1])
    commonItems.formIntersection(lines[i + 2])
    
    let priorities: [Character: Int] = Dictionary(uniqueKeysWithValues: zip("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", 1...52))
    
    for item in commonItems {
        sum += priorities[item] ?? 0
    }
}

print(sum)
