import Foundation

func checkAndCompleteLine(_ line: String) -> (Int, Bool) {
    let pairings: [Character: Character] = [")": "(", "]": "[", "}": "{", ">": "<"]
    let scoreValues: [Character: Int] = [")": 1, "]": 2, "}": 3, ">": 4]
    let opening: [Character] = ["(", "[", "{", "<"]
    let closing: [Character] = [")", "]", "}", ">"]
    var stack: [Character] = []

    for char in line {
        if opening.contains(char) {
            stack.append(char)
        } else if closing.contains(char) {
            if stack.isEmpty || stack.last != pairings[char] {
                return (0, false) // corrupted line
            }
            stack.removeLast() // pop from stack
        }
    }

    if stack.isEmpty {
        return (0, false) // line is not incomplete
    }

    // Calculate score for incomplete line
    var score = 0
    for char in stack.reversed() {
        score *= 5
        score += scoreValues[getClosingChar(char)]!
    }
    return (score, true)
}

func getClosingChar(_ openingChar: Character) -> Character {
    switch openingChar {
    case "(":
        return ")"
    case "[":
        return "]"
    case "{":
        return "}"
    case "<":
        return ">"
    default:
        return " "
    }
}

do {
    let fileURL = URL(fileURLWithPath: "input.txt")
    let content = try String(contentsOf: fileURL, encoding: .utf8)
    let lines = content.components(separatedBy: "\n")
    var scores: [Int] = []

    for line in lines {
        let (score, incomplete) = checkAndCompleteLine(line)
        if incomplete {
            scores.append(score)
        }
    }

    scores.sorted()
    let middleScore = scores[scores.count / 2]
    print(middleScore)
} catch {
    print("Error reading file: \(error)")
}