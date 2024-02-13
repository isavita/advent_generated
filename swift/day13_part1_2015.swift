
import Foundation

extension Array {
    var permutations: [[Element]] {
        guard count > 1 else { return [self] }
        
        var permutations: [[Element]] = []
        
        for (index, element) in enumerated() {
            var remaining = self
            remaining.remove(at: index)
            let subPermutations = remaining.permutations
            for subPermutation in subPermutations {
                permutations.append([element] + subPermutation)
            }
        }
        
        return permutations
    }
}

func calculateHappinessChange(seating: [String], happiness: [String: [String: Int]]) -> Int {
    var totalHappiness = 0
    for i in 0..<seating.count {
        let person = seating[i]
        let left = seating[(i + seating.count - 1) % seating.count]
        let right = seating[(i + 1) % seating.count]
        totalHappiness += happiness[person]![left]! + happiness[person]![right]!
    }
    return totalHappiness
}

func parseInput(_ input: String) -> ([String], [String: [String: Int]]) {
    var happiness: [String: [String: Int]] = [:]
    var people: Set<String> = []
    
    let lines = input.components(separatedBy: .newlines)
    for line in lines {
        let parts = line.components(separatedBy: " ")
        let person = parts[0]
        let neighbor = parts[10].replacingOccurrences(of: ".", with: "")
        let value = Int(parts[3])! * (parts[2] == "gain" ? 1 : -1)
        
        if happiness[person] == nil {
            happiness[person] = [:]
        }
        happiness[person]![neighbor] = value
        people.insert(person)
    }
    
    return (Array(people), happiness)
}

if let input = try? String(contentsOfFile: "input.txt") {
    let (people, happiness) = parseInput(input)
    
    var maxHappiness = Int.min
    for permutation in people.permutations {
        let happinessChange = calculateHappinessChange(seating: permutation, happiness: happiness)
        maxHappiness = max(maxHappiness, happinessChange)
    }
    
    print(maxHappiness)
}
