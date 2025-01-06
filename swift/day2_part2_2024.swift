
import Foundation

func isSafe(levels: [Int]) -> Bool {
    guard levels.count > 1 else { return true }

    var isIncreasing = levels[1] > levels[0]
    var isDecreasing = levels[1] < levels[0]

    for i in 0..<levels.count - 1 {
        let diff = abs(levels[i+1] - levels[i])
        if diff < 1 || diff > 3 {
            return false
        }
        if levels[i+1] > levels[i] {
            if isDecreasing {
                return false
            }
            isIncreasing = true
        } else if levels[i+1] < levels[i] {
            if isIncreasing {
                return false
            }
            isDecreasing = true
        } else {
            return false
        }
    }
    return true
}

func isSafeWithDampener(levels: [Int]) -> Bool {
    if isSafe(levels: levels) {
        return true
    }
    
    for i in 0..<levels.count {
        var tempLevels = levels
        tempLevels.remove(at: i)
        if isSafe(levels: tempLevels) {
            return true
        }
    }
    return false
}

func solve() -> (Int, Int) {
    guard let fileURL = Bundle.main.url(forResource: "input", withExtension: "txt"),
          let fileContents = try? String(contentsOf: fileURL) else {
        fatalError("Could not read input file")
    }

    let lines = fileContents.components(separatedBy: .newlines).filter { !$0.isEmpty }
    var safeCountPart1 = 0
    var safeCountPart2 = 0

    for line in lines {
        let levels = line.components(separatedBy: " ").compactMap { Int($0) }
        if isSafe(levels: levels) {
            safeCountPart1 += 1
        }
        if isSafeWithDampener(levels: levels) {
            safeCountPart2 += 1
        }
    }
    return (safeCountPart1, safeCountPart2)
}

let (part1, part2) = solve()
print("Part 1: \(part1)")
print("Part 2: \(part2)")
