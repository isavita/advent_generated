
import Foundation

func readInput(_ filename: String) -> (String, [String: String]) {
    let fileURL = URL(fileURLWithPath: filename)
    let contents = try! String(contentsOf: fileURL)
    let lines = contents.components(separatedBy: "\n")
    
    let template = lines[0]
    var rules = [String: String]()
    for i in 1..<lines.count {
        let line = lines[i]
        if line.isEmpty {
            continue
        }
        let parts = line.components(separatedBy: " -> ")
        rules[parts[0]] = parts[1]
    }
    
    return (template, rules)
}

let (template, rules) = readInput("input.txt")
var pairCounts = [String: Int64]()
for i in 0..<(template.count - 1) {
    let startIndex = template.index(template.startIndex, offsetBy: i)
    let endIndex = template.index(startIndex, offsetBy: 2)
    let pair = String(template[startIndex..<endIndex])
    pairCounts[pair, default: 0] += 1
}

for step in 0..<40 {
    var newPairCounts = [String: Int64]()
    for (pair, count) in pairCounts {
        if let insert = rules[pair] {
            newPairCounts[String(pair[pair.startIndex]) + insert, default: 0] += count
            newPairCounts[insert + String(pair[pair.index(before: pair.endIndex)]), default: 0] += count
        } else {
            newPairCounts[pair, default: 0] += count
        }
    }
    pairCounts = newPairCounts
}

var elementCounts = [Character: Int64]()
for (pair, count) in pairCounts {
    elementCounts[pair[pair.startIndex], default: 0] += count
}
elementCounts[template.last!, default: 0] += 1

var maxCount: Int64 = 0
var minCount: Int64 = Int64.max
for (_, count) in elementCounts {
    maxCount = max(maxCount, count)
    minCount = min(minCount, count)
}

print(maxCount - minCount)
