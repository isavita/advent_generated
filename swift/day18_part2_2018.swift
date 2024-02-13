
import Foundation

let Open: Character = "."
let Trees: Character = "|"
let Lumberyard: Character = "#"
let Size: Int = 50

func readInput(_ filename: String) -> [[Character]] {
    let fileURL = URL(fileURLWithPath: filename)
    let contents = try! String(contentsOf: fileURL)
    var grid = [[Character]]()
    let lines = contents.components(separatedBy: .newlines)
    for line in lines {
        var row = [Character]()
        for char in line {
            row.append(char)
        }
        grid.append(row)
    }
    return grid
}

func transform(_ grid: [[Character]]) -> [[Character]] {
    var newGrid = [[Character]](repeating: [Character](repeating: Open, count: Size), count: Size)
    for i in 0..<Size {
        for j in 0..<Size {
            newGrid[i][j] = nextAcreState(grid, i, j)
        }
    }
    return newGrid
}

func nextAcreState(_ grid: [[Character]], _ i: Int, _ j: Int) -> Character {
    switch grid[i][j] {
    case Open:
        if countAdjacent(grid, i, j, Trees) >= 3 {
            return Trees
        }
    case Trees:
        if countAdjacent(grid, i, j, Lumberyard) >= 3 {
            return Lumberyard
        }
    case Lumberyard:
        if countAdjacent(grid, i, j, Lumberyard) >= 1 && countAdjacent(grid, i, j, Trees) >= 1 {
            return Lumberyard
        }
        return Open
    default:
        break
    }
    return grid[i][j]
}

func countAdjacent(_ grid: [[Character]], _ i: Int, _ j: Int, _ acreType: Character) -> Int {
    var count = 0
    for x in -1...1 {
        for y in -1...1 {
            if x == 0 && y == 0 {
                continue
            }
            if i+x >= 0 && i+x < Size && j+y >= 0 && j+y < Size && grid[i+x][j+y] == acreType {
                count += 1
            }
        }
    }
    return count
}

func countResources(_ grid: [[Character]]) -> (Int, Int) {
    var wooded = 0
    var lumberyards = 0
    for row in grid {
        for char in row {
            switch char {
            case Trees:
                wooded += 1
            case Lumberyard:
                lumberyards += 1
            default:
                break
            }
        }
    }
    return (wooded, lumberyards)
}

var grid = readInput("input.txt")
var seenStates = [String: Int]()
var cycleStart = 0
var cycleLength = 0

for minute in 0... {
    let state = gridToString(grid)
    if let seenMinute = seenStates[state] {
        cycleStart = seenMinute
        cycleLength = minute - seenMinute
        break
    }
    seenStates[state] = minute
    grid = transform(grid)
}

let remainingMinutes = (1000000000 - cycleStart) % cycleLength
for _ in 0..<remainingMinutes {
    grid = transform(grid)
}

let resources = countResources(grid)
print(resources.0 * resources.1)

func gridToString(_ grid: [[Character]]) -> String {
    var result = ""
    for row in grid {
        result += String(row) + "\n"
    }
    return result
}
