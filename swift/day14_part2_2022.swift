
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)

var matrix = parseInput(input)
var originCol = 0
for (i, c) in matrix[0].enumerated() {
    if c == "+" {
        originCol = i
    }
    matrix[matrix.count - 1][i] = "#"
}

var ans = 0
while !dropSand(matrix: &matrix, originCol: originCol) {
    ans += 1
    if matrix[0][originCol] == "o" {
        break
    }
}

print(ans)

func parseInput(_ input: String) -> [[String]] {
    var coordSets: [[[Int]]] = []
    var lowestCol = Int.max
    var highestRow = 0

    for line in input.components(separatedBy: "\n") {
        var coords: [[Int]] = []
        let rawCoords = line.components(separatedBy: " -> ")
        for rawCoord in rawCoords {
            let rawNums = rawCoord.components(separatedBy: ",")
            let col = Int(rawNums[0])!
            let row = Int(rawNums[1])!
            let coord = [col, row]
            coords.append(coord)

            lowestCol = min(lowestCol, col)
            highestRow = max(highestRow, row)
        }
        coordSets.append(coords)
    }

    let ExtraLeftSpace = 200
    var highestCol = 0
    for (s, set) in coordSets.enumerated() {
        for i in 0..<set.count {
            coordSets[s][i][0] -= lowestCol - ExtraLeftSpace
            highestCol = max(highestCol, coordSets[s][i][0])
        }
    }

    var matrix: [[String]] = Array(repeating: Array(repeating: "", count: highestCol + ExtraLeftSpace * 2), count: highestRow + 3)

    for set in coordSets {
        for i in 1..<set.count {
            let cols = [set[i-1][0], set[i][0]]
            let rows = [set[i-1][1], set[i][1]]

            let sortedCols = cols.sorted()
            let sortedRows = rows.sorted()

            if sortedCols[0] == sortedCols[1] {
                for r in sortedRows[0]...sortedRows[1] {
                    matrix[r][sortedCols[0]] = "#"
                }
            } else if sortedRows[0] == sortedRows[1] {
                for c in sortedCols[0]...sortedCols[1] {
                    matrix[sortedRows[0]][c] = "#"
                }
            }
        }
    }

    let originCol = 500 - lowestCol + ExtraLeftSpace
    matrix[0][originCol] = "+"

    for i in 0..<matrix.count {
        for j in 0..<matrix[i].count {
            if matrix[i][j] == "" {
                matrix[i][j] = "."
            }
        }
    }

    return matrix
}

func dropSand(matrix: inout [[String]], originCol: Int) -> Bool {
    var r = 0
    var c = originCol

    while r < matrix.count - 1 {
        let below = matrix[r + 1][c]
        let diagonallyLeft = matrix[r + 1][c - 1]
        let diagonallyRight = matrix[r + 1][c + 1]

        if below == "." {
            r += 1
        } else if diagonallyLeft == "." {
            r += 1
            c -= 1
        } else if diagonallyRight == "." {
            r += 1
            c += 1
        } else {
            matrix[r][c] = "o"
            return false
        }
    }

    return true
}
