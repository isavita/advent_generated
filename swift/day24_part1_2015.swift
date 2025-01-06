
import Foundation

func findQuantumEntanglement(weights: [Int], groups: Int) -> Int {
    let totalWeight = weights.reduce(0, +)
    let targetWeight = totalWeight / groups

    func canSplit(remaining: [Int], target: Int, count: Int) -> Bool {
        if target == 0 { return count == groups - 1 }
        if target < 0 || remaining.isEmpty { return false }

        for i in 0..<remaining.count {
            var newRemaining = remaining
            let weight = newRemaining.remove(at: i)
            if canSplit(remaining: newRemaining, target: target - weight, count: count + (target - weight == 0 ? 1 : 0)) {
                return true
            }
        }
        return false
    }

    var bestQE = Int.max
    var minPackages = Int.max

    for i in 1..<weights.count {
        for combo in combinations(of: weights, size: i) {
            if combo.reduce(0, +) == targetWeight {
                if canSplit(remaining: weights.filter { !combo.contains($0) }, target: targetWeight, count: 1) {
                    if i < minPackages {
                        minPackages = i
                        bestQE = combo.reduce(1, *)
                    } else if i == minPackages {
                        bestQE = min(bestQE, combo.reduce(1, *))
                    }
                }
            }
        }
        if bestQE != Int.max {
            break
        }
    }

    return bestQE
}

func combinations<T>(of array: [T], size: Int) -> [[T]] {
    guard size <= array.count else { return [] }
    if size == 0 { return [[]] }
    if size == array.count { return [array] }

    let first = array[0]
    let rest = Array(array.dropFirst())
    let combosWithoutFirst = combinations(of: rest, size: size)
    let combosWithFirst = combinations(of: rest, size: size - 1).map { [first] + $0 }
    return combosWithFirst + combosWithoutFirst
}

let fileURL = URL(fileURLWithPath: "input.txt")
guard let input = try? String(contentsOf: fileURL) else {
    fatalError("Could not read input file")
}

let weights = input.trimmingCharacters(in: .whitespacesAndNewlines)
    .split(separator: "\n")
    .compactMap { Int($0) }

let quantumEntanglement = findQuantumEntanglement(weights: weights, groups: 3)
print(quantumEntanglement)
