
import Foundation

func solve() {
    guard let input = try? String(contentsOfFile: "input.txt") else { return }
    let lines = input.split(separator: "\n")

    var grid: [[Character]] = []
    var moves = ""
    var readingMap = true

    for line in lines {
        if readingMap {
            if line.contains("#") {
                grid.append(Array(line))
            } else {
                readingMap = false
                moves += line
            }
        } else {
            moves += line
        }
    }

    let rows = grid.count
    let cols = grid[0].count
    var robotR = 0
    var robotC = 0

    for r in 0..<rows {
        for c in 0..<cols {
            if grid[r][c] == "@" {
                robotR = r
                robotC = c
                break
            }
        }
    }

    let dirs: [Character: (Int, Int)] = [
        "^": (-1, 0),
        "v": (1, 0),
        "<": (0, -1),
        ">": (0, 1)
    ]

    func pushBoxes(r: Int, c: Int, dr: Int, dc: Int) -> Bool {
        let nr = r + dr
        let nc = c + dc
        if grid[nr][nc] == "#" { return false }
        if grid[nr][nc] == "O" {
            if !pushBoxes(r: nr, c: nc, dr: dr, dc: dc) { return false }
        }
        if grid[nr][nc] == "." {
            grid[nr][nc] = "O"
            grid[r][c] = "."
            return true
        }
        return false
    }

    for move in moves {
        let (dr, dc) = dirs[move]!
        let nr = robotR + dr
        let nc = robotC + dc
        if grid[nr][nc] == "#" { continue }
        if grid[nr][nc] == "O" {
            if !pushBoxes(r: nr, c: nc, dr: dr, dc: dc) { continue }
        }
        if grid[nr][nc] == "." || grid[nr][nc] == "O" {
            grid[robotR][robotC] = "."
            grid[nr][nc] = "@"
            robotR = nr
            robotC = nc
        }
    }

    var totalSum = 0
    for r in 0..<rows {
        for c in 0..<cols {
            if grid[r][c] == "O" {
                totalSum += r * 100 + c
            }
        }
    }

    print(totalSum)
}

solve()
