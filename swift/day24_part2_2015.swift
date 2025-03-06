
import Foundation

func findIdealConfiguration(packages: [Int], groupsCount: Int) -> Int {
    let totalWeight = packages.reduce(0, +)
    let targetWeight = totalWeight / groupsCount

    func quantumEntanglement(_ group: [Int]) -> Int {
        return group.reduce(1, *)
    }

    func findCombinations(startIndex: Int, currentCombination: [Int], currentWeight: Int, combinations: inout [[Int]]) {
        if currentWeight == targetWeight {
            combinations.append(currentCombination)
            return
        }

        if currentWeight > targetWeight || startIndex >= packages.count {
            return
        }

        for i in startIndex..<packages.count {
            var newCombination = currentCombination
            newCombination.append(packages[i])
            findCombinations(startIndex: i + 1, currentCombination: newCombination, currentWeight: currentWeight + packages[i], combinations: &combinations)
        }
    }
    
    func canSplitRemaining(remaining: [Int], count: Int) -> Bool {
        if count == 1 {
            return remaining.reduce(0, +) == targetWeight
        }
        
        let remainingTotal = remaining.reduce(0,+)
        if remainingTotal % count != 0 { return false }
        let subTarget = remainingTotal / count
        
        func backtrack(startIndex: Int, currentSum: Int, current: [Int], remainingCount: Int) -> Bool{
            if remainingCount == 0 { return true}
            
            if currentSum == subTarget {
                let nextRemaining = remaining.filter{ !current.contains($0) }
                return backtrack(startIndex: 0, currentSum: 0, current: [], remainingCount: remainingCount - 1)
            }
            
            if currentSum > subTarget || startIndex >= remaining.count { return false }
            
            for i in startIndex..<remaining.count {
                if backtrack(startIndex: i + 1, currentSum: currentSum + remaining[i], current: current + [remaining[i]], remainingCount: remainingCount) {
                    return true
                }
            }
            
            return false
        }
            
        return backtrack(startIndex: 0, currentSum: 0, current: [], remainingCount: count - 1)
    }

    var validCombinations: [[Int]] = []
    findCombinations(startIndex: 0, currentCombination: [], currentWeight: 0, combinations: &validCombinations)
    
    validCombinations.sort { $0.count < $1.count }
    
    var minLength = validCombinations.first!.count
    var bestQE = Int.max

    for combination in validCombinations {
        if combination.count > minLength {
            break // Optimization, since they are sorted
        }
        
        let remainingPackages = packages.filter { !combination.contains($0) }
        if canSplitRemaining(remaining: remainingPackages, count: groupsCount - 1) {
            minLength = combination.count
            bestQE = min(bestQE, quantumEntanglement(combination))
        }
    }

    return bestQE
}


func main() {
    do {
        let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let packages = input.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: "\n").compactMap { Int($0) }

        // Part 1
        let bestQE1 = findIdealConfiguration(packages: packages, groupsCount: 3)
        print("Part 1: \(bestQE1)")

        // Part 2
        let bestQE2 = findIdealConfiguration(packages: packages, groupsCount: 4)
        print("Part 2: \(bestQE2)")

    } catch {
        print("Error reading input file: \(error)")
    }
}

main()
