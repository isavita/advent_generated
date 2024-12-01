import Foundation

let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
let lines = input.components(separatedBy: .newlines).filter { !$0.isEmpty }

var leftList: [Int] = []
var rightCounts: [Int: Int] = [:]

for line in lines {
    let numbers = line.split(separator: " ")
        .filter { !$0.isEmpty }
        .compactMap { Int($0) }
    
    if numbers.count >= 2 {
        leftList.append(numbers[0])
        rightCounts[numbers[1], default: 0] += 1
    }
}

let similarityScore = leftList.reduce(0) { sum, num in
    sum + (num * (rightCounts[num] ?? 0))
}

print("Similarity score:", similarityScore)
