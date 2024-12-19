
import Foundation

let dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

func solve() -> Int64 {
    guard let data = try? String(contentsOfFile: "input.txt") else { return 0 }
    let lines = data.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n")
    let nr = lines.count
    let nc = lines[0].count
    var grid = [[Int]](repeating: [Int](repeating: 0, count: nc), count: nr)
    for i in 0..<nr {
        for (j, char) in lines[i].enumerated() {
            grid[i][j] = Int(String(char))!
        }
    }

    var dp = [[Int64]](repeating: [Int64](repeating: -1, count: nc), count: nr)

    func dfs(r: Int, c: Int) -> Int64 {
        if dp[r][c] != -1 {
            return dp[r][c]
        }
        let h = grid[r][c]
        if h == 9 {
            dp[r][c] = 1
            return 1
        }
        var sum: Int64 = 0
        for d in dirs {
            let nr2 = r + d.0
            let nc2 = c + d.1
            if nr2 < 0 || nr2 >= nr || nc2 < 0 || nc2 >= nc { continue }
            if grid[nr2][nc2] == h + 1 {
                sum += dfs(r: nr2, c: nc2)
            }
        }
        dp[r][c] = sum
        return sum
    }

    var total: Int64 = 0
    for r in 0..<nr {
        for c in 0..<nc {
            if grid[r][c] == 0 {
                total += dfs(r: r, c: c)
            }
        }
    }
    return total
}

print(solve())
