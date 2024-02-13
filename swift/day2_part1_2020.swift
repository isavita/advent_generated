
import Foundation

let input = try String(contentsOfFile: "input.txt")
let lines = input.components(separatedBy: .newlines)

var validCount = 0

for line in lines {
    let components = line.components(separatedBy: CharacterSet(charactersIn: "- :"))
    let minCount = Int(components[0])!
    let maxCount = Int(components[1])!
    let targetChar = Character(components[2])
    let password = components[4]
    
    let count = password.filter { $0 == targetChar }.count
    if count >= minCount && count <= maxCount {
        validCount += 1
    }
}

print(validCount)
