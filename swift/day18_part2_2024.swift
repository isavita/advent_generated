
import Foundation

func solve() {
    guard let data = try? String(contentsOfFile: "input.txt") else {
        fatalError("Could not read input.txt")
    }

    let bytePositions = data.components(separatedBy: .newlines)
        .filter { !$0.isEmpty }
        .map { line -> (Int, Int) in
            let parts = line.components(separatedBy: ",").compactMap { Int($0) }
            return (parts[0], parts[1])
        }

    let gridSize = 71

    func isValid(r: Int, c: Int, grid: [[Character]]) -> Bool {
        return r >= 0 && r < gridSize && c >= 0 && c < gridSize && grid[r][c] == "."
    }

    func bfs(grid: [[Character]]) -> Int {
        var q: [(Int, Int, Int)] = [(0, 0, 0)]
        var visited = Set<[Int]>()
        visited.insert([0, 0])
        var head = 0

        while head < q.count {
            let (r, c, dist) = q[head]
            head += 1

            if r == gridSize - 1 && c == gridSize - 1 {
                return dist
            }

            let directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]
            for (dr, dc) in directions {
                let nr = r + dr
                let nc = c + dc
                if isValid(r: nr, c: nc, grid: grid), !visited.contains([nr, nc]) {
                    visited.insert([nr, nc])
                    q.append((nr, nc, dist + 1))
                }
            }
        }
        return -1
    }

    // Part 1
    var grid1 = [[Character]](repeating: [Character](repeating: ".", count: gridSize), count: gridSize)
    for (x, y) in bytePositions.prefix(1024) {
        grid1[y][x] = "#"
    }

    let part1Result = bfs(grid: grid1)
    print(part1Result)

    // Part 2
    var grid2 = [[Character]](repeating: [Character](repeating: ".", count: gridSize), count: gridSize)
    for (i, (x, y)) in bytePositions.enumerated() {
        grid2[y][x] = "#"
        if bfs(grid: grid2) == -1 {
            print("\(x),\(y)")
            break
        }
    }
}

solve()
