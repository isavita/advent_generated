
import Foundation

func solve(input: String) -> Int {
    let lines = parseInput(input: input)
    var total = 0
    for line in lines {
        total += doMaths(input: line)
    }
    return total
}

func parseInput(input: String) -> [[String]] {
    let lines = input.components(separatedBy: "\n")
    return lines.map { $0.replacingOccurrences(of: " ", with: "").map { String($0) } }
}

func doMaths(input: [String]) -> Int {
    var stackOpenIndices: [Int] = []
    var stackFlattened: [String] = []
    for i in 0..<input.count {
        stackFlattened.append(input[i])
        switch input[i] {
        case "(":
            stackOpenIndices.append(stackFlattened.count - 1)
        case ")":
            let openIndex = stackOpenIndices.removeLast()
            let sliToFlatten = Array(stackFlattened[openIndex + 1..<stackFlattened.count - 1])
            stackFlattened[openIndex] = calcFlatSlicePart(input: sliToFlatten)
            stackFlattened = Array(stackFlattened[0..<openIndex + 1])
        default:
            break
        }
    }
    return Int(calcFlatSlicePart(input: stackFlattened))!
}

func calcFlatSlicePart(input: [String]) -> String {
    var mutableInput = input
    var i = 1
    while i < mutableInput.count - 1 {
        if mutableInput[i] == "+" {
            let toLeft = mutableInput[i - 1]
            let toRight = mutableInput[i + 1]
            if let leftNum = Int(toLeft), let rightNum = Int(toRight) {
                mutableInput[i - 1] = String(leftNum + rightNum)
                mutableInput.removeSubrange(i...i + 1)
                continue
            }
        }
        i += 1
    }
    i = 1
    while i < mutableInput.count - 1 {
        if mutableInput[i] == "*" {
            let toLeft = mutableInput[i - 1]
            let toRight = mutableInput[i + 1]
            if let leftNum = Int(toLeft), let rightNum = Int(toRight) {
                mutableInput[i - 1] = String(leftNum * rightNum)
                mutableInput.removeSubrange(i...i + 1)
                continue
            }
        }
        i += 1
    }
    return mutableInput[0]
}

let fileURL = URL(fileURLWithPath: "input.txt")
do {
    let fileContents = try String(contentsOf: fileURL)
    let input = fileContents.trimmingCharacters(in: .whitespacesAndNewlines)
    let ans = solve(input: input)
    print(ans)
} catch {
    print("Error reading from file: \(error)")
}
