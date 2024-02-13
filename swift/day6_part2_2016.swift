
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)

let lines = input.components(separatedBy: "\n")
let messageLength = lines[0].count
var errorCorrectedMessage = ""
var originalMessage = ""

for i in 0..<messageLength {
    var charCount: [Character: Int] = [:]
    for line in lines {
        let char = line[line.index(line.startIndex, offsetBy: i)]
        charCount[char, default: 0] += 1
    }
    
    let mostCommonChar = charCount.max { $0.value < $1.value }!.key
    let leastCommonChar = charCount.min { $0.value < $1.value }!.key
    
    errorCorrectedMessage.append(mostCommonChar)
    originalMessage.append(leastCommonChar)
}

print(errorCorrectedMessage)
print(originalMessage)
