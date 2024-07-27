
import Foundation

func main() {
    guard let data = try? String(contentsOfFile: "input.txt") else { return }
    let lines = data.split(separator: "\n").map { Array($0) }
    var grid = lines
    print(findSafeStep(&grid))
}

func findSafeStep(_ grid: inout [[Character]]) -> Int {
    var step = 0
    while true {
        let eastMoved = moveEast(&grid)
        let southMoved = moveSouth(&grid)
        step += 1
        if !eastMoved && !southMoved { break }
    }
    return step
}

func moveEast(_ grid: inout [[Character]]) -> Bool {
    let height = grid.count
    let width = grid[0].count
    var moved = false
    var oldPositions = grid

    for y in 0..<height {
        for x in 0..<width {
            if grid[y][x] == ">" {
                let nextX = (x + 1) % width
                if grid[y][nextX] == "." {
                    oldPositions[y][x] = "."
                    oldPositions[y][nextX] = ">"
                    moved = true
                }
            }
        }
    }
    grid = oldPositions
    return moved
}

func moveSouth(_ grid: inout [[Character]]) -> Bool {
    let height = grid.count
    let width = grid[0].count
    var moved = false
    var oldPositions = grid

    for x in 0..<width {
        for y in 0..<height {
            if grid[y][x] == "v" {
                let nextY = (y + 1) % height
                if grid[nextY][x] == "." {
                    oldPositions[y][x] = "."
                    oldPositions[nextY][x] = "v"
                    moved = true
                }
            }
        }
    }
    grid = oldPositions
    return moved
}

main()
