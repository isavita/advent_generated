
import Foundation

let input = try String(contentsOfFile: "input.txt")
var grid = input.components(separatedBy: .newlines).filter { !$0.isEmpty }.map { Array($0) }

func getAdjacentAcres(_ x: Int, _ y: Int) -> [Character] {
    var adjacentAcres: [Character] = []
    for i in -1...1 {
        for j in -1...1 {
            if i == 0 && j == 0 {
                continue
            }
            let newX = x + i
            let newY = y + j
            if newX >= 0 && newX < grid.count && newY >= 0 && newY < grid[0].count {
                adjacentAcres.append(grid[newX][newY])
            }
        }
    }
    return adjacentAcres
}

func getNextState(_ x: Int, _ y: Int) -> Character {
    let current = grid[x][y]
    let adjacentAcres = getAdjacentAcres(x, y)
    let treeCount = adjacentAcres.filter { $0 == "|" }.count
    let lumberyardCount = adjacentAcres.filter { $0 == "#" }.count
    
    if current == "." && treeCount >= 3 {
        return "|"
    } else if current == "|" && lumberyardCount >= 3 {
        return "#"
    } else if current == "#" {
        return (treeCount >= 1 && lumberyardCount >= 1) ? "#" : "."
    }
    
    return current
}

func getTotalResourceValue() -> Int {
    var woodedAcres = 0
    var lumberyards = 0
    
    for _ in 1...10 {
        var newGrid = grid
        for i in 0..<grid.count {
            for j in 0..<grid[0].count {
                newGrid[i][j] = getNextState(i, j)
            }
        }
        grid = newGrid
    }
    
    for row in grid {
        woodedAcres += row.filter { $0 == "|" }.count
        lumberyards += row.filter { $0 == "#" }.count
    }
    
    return woodedAcres * lumberyards
}

print(getTotalResourceValue())
