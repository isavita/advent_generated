import Foundation

func applyInsertion(polymer: String, rules: [String: String]) -> String {
    var newPolymer = ""
    for i in 0..<polymer.count-1 {
        let index = polymer.index(polymer.startIndex, offsetBy: i)
        let char = String(polymer[index])
        newPolymer.append(char)
        let key = String(polymer[index...]).prefix(2)
        if let insert = rules[String(key)] {
            newPolymer.append(insert)
        }
    }
    newPolymer.append(polymer.last!)
    return newPolymer
}

func countElements(polymer: String) -> [Character: Int] {
    var counts: [Character: Int] = [:]
    for char in polymer {
        counts[char, default: 0] += 1
    }
    return counts
}

func minMax(counts: [Character: Int]) -> (min: Int, max: Int) {
    var min = Int.max
    var max = Int.min
    for count in counts.values {
        if count < min {
            min = count
        }
        if count > max {
            max = count
        }
    }
    return (min, max)
}

do {
    let fileURL = URL(fileURLWithPath: "input.txt")
    let content = try String(contentsOf: fileURL, encoding: .utf8)
    let lines = content.components(separatedBy: "\n")
    var polymer = lines[0]
    var rules: [String: String] = [:]
    for line in lines[2...] {
        if line.isEmpty { continue }
        let parts = line.components(separatedBy: " -> ")
        rules[parts[0]] = parts[1]
    }
    
    for _ in 0..<10 {
        polymer = applyInsertion(polymer: polymer, rules: rules)
    }
    
    let counts = countElements(polymer: polymer)
    let (min, max) = minMax(counts: counts)
    print(max - min)
} catch {
    print("Error: \(error)")
}