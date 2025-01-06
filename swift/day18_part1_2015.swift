
import Foundation

func solve() {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        print("Error reading input file.")
        return
    }

    var grid = input.trimmingCharacters(in: .whitespacesAndNewlines)
        .split(separator: "\n")
        .map { $0.map { $0 == "#" } }

    func countOnNeighbors(row: Int, col: Int) -> Int {
        var count = 0
        for r in max(0, row - 1)...min(grid.count - 1, row + 1) {
            for c in max(0, col - 1)...min(grid[0].count - 1, col + 1) {
                if (r != row || c != col) && grid[r][c] {
                    count += 1
                }
            }
        }
        return count
    }

    for _ in 0..<100 {
        var newGrid = grid
        for row in 0..<grid.count {
            for col in 0..<grid[0].count {
                let onNeighbors = countOnNeighbors(row: row, col: col)
                if grid[row][col] {
                    newGrid[row][col] = (onNeighbors == 2 || onNeighbors == 3)
                } else {
                    newGrid[row][col] = (onNeighbors == 3)
                }
            }
        }
        grid = newGrid
    }

    let onCount = grid.flatMap { $0 }.filter { $0 }.count
    print(onCount)
}

solve()
