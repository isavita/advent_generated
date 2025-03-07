
import Foundation

func solve() -> Int {
    guard let data = try? String(contentsOfFile: "input.txt", encoding: .utf8) else {
        return 0
    }
    let grid = data.split(separator: "\n").map { String($0) }
    let rows = grid.count
    let cols = grid[0].count
    let word = "XMAS"
    let wordLength = word.count
    let dirs = [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)]
    var count = 0
    let wordChars = Array(word)

    for i in 0..<rows {
        for j in 0..<cols {
            if grid[i].startIndex == grid[i].startIndex && grid[i][grid[i].index(grid[i].startIndex, offsetBy: j)] == wordChars[0] {
                for dir in dirs {
                    var x = i
                    var y = j
                    var k = 0
                    while x >= 0 && x < rows && y >= 0 && y < cols && k < wordLength && grid[x][grid[x].index(grid[x].startIndex, offsetBy: y)] == wordChars[k] {
                        x += dir.0
                        y += dir.1
                        k += 1
                    }
                    if k == wordLength {
                        count += 1
                    }
                }
            }
        }
    }

    return count
}

print(solve())
