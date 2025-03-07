
import Foundation

func countWays(design: String, patterns: [String]) -> Int {
    let n = design.count
    var dp = [Int](repeating: 0, count: n + 1)
    dp[0] = 1
    let designChars = Array(design)

    for i in 1...n {
        for p in patterns {
            let lp = p.count
            if i >= lp {
                let startIndex = designChars.index(designChars.startIndex, offsetBy: i - lp)
                let endIndex = designChars.index(startIndex, offsetBy: lp)
                if Array(designChars[startIndex..<endIndex]) == Array(p) {
                    dp[i] += dp[i - lp]
                }
            }
        }
    }
    return dp[n]
}

func solve() {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        return
    }

    let lines = input.components(separatedBy: .newlines)
    let availablePatterns = lines[0].split(separator: ",").map { $0.trimmingCharacters(in: .whitespaces) }
    
    var totalWays = 0
    for i in 2..<lines.count {
      let design = lines[i].trimmingCharacters(in: .whitespacesAndNewlines)
        if !design.isEmpty {
            totalWays += countWays(design: design, patterns: availablePatterns)
        }
    }
    print(totalWays)
}

solve()
