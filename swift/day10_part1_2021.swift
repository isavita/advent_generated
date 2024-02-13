
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let content = try String(contentsOf: fileURL)
var totalScore = 0

let pairings: [Character: Character] = [")": "(", "]": "[", "}": "{", ">": "<"]
let scores: [Character: Int] = [")": 3, "]": 57, "}": 1197, ">": 25137]

for line in content.components(separatedBy: .newlines) {
    let result = checkLine(line: line, pairings: pairings, scores: scores)
    if result.corrupted {
        totalScore += result.score
    }
}

print(totalScore)

func checkLine(line: String, pairings: [Character: Character], scores: [Character: Int]) -> (score: Int, corrupted: Bool) {
    var stack: [Character] = []
    
    for char in line {
        switch char {
        case "(", "[", "{", "<":
            stack.append(char)
        case ")", "]", "}", ">":
            if stack.isEmpty || stack.last != pairings[char] {
                return (score: scores[char] ?? 0, corrupted: true)
            }
            stack.removeLast()
        default:
            break
        }
    }
    
    return (score: 0, corrupted: false)
}
